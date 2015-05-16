#include <assert.h>
#include <string.h>
#include <stdio.h>
#include "symbol.h"
#include "ast_print.h"

static void print_indent(void);
static void print_typed_head(const char* head, const struct type* type);
static void print_head(const char* head);
static void print_typed_leaf(const char* head, const struct type* type);
static void print_leaf(const char* head);
static void print_rparen(void);

static const char* op_to_str(const char* op, bool unary, bool mut);

static void symbol_print(Symbol id);
static void item_print(const struct item*);
static void stmt_print(const struct stmt*);
static void pat_print(const struct pat*);
static void exp_print(const struct exp*);
static void pair_print(const struct pair*);
static void type_print(const struct type*);

static void llvm_stmt(const struct stmt* stmt);
static void llvm_binary(const struct exp* exp, int leg);
void llvm_item(const struct item*);
void llvm_exp(const struct exp*);
const char* llvm_get_type(const struct type* type);
void llvm_print_type(const struct type* type);
const char* llvm_op_to_str(const char* op);
static void llvm_strings(const struct item* item);

static int last_register;
static struct type* last_type;
static int last_label;
static GList* last_args;
static int last_if;
static int num_to_print = 0; 
static int last_string = 2; 

#define INDENT "  "
static int indent_level;
static void print_indent(void) {
      puts("");
      for (int i = 0; i != indent_level; ++i) {
            printf(INDENT);
      }
}
static void print_typed_head(const char* head, const struct type* type) {
      print_indent();
      printf("(%s:", head);
      type_print_pretty(type);
      ++indent_level;
}
static void print_head(const char* head) {
      print_indent();
      printf("(%s", head);
      ++indent_level;
}
static void print_typed_leaf(const char* head, const struct type* type) {
      print_indent();
      printf("(%s:", head);
      type_print_pretty(type);
      printf(")");
}
static void print_leaf(const char* head) {
      print_indent();
      printf("(%s)", head);
}
static void print_rparen(void) {
      --indent_level;
      printf(")");
}

// *** Symbols ***

static void symbol_print(Symbol id) {
      print_head("id");
      print_leaf(symbol_to_str(id));
      print_rparen();
}

// *** Crate ***

void crate_print(const GList* items) {

      indent_level = 0;

      struct type* type = type_ok();

      for (const GList* i = items; i; i = i->next) {
            struct item* item = i->data;
            if (item->type != type_ok()) {
                  type = type_error();
                  break;
            }
      }

      print_typed_head("crate", type);
      print_head("items");
      g_list_foreach((GList*)items, (GFunc)item_print, NULL);
      print_rparen();
      print_rparen();
      puts("");
}

// *** Items ***

static void item_print(const struct item* item) {
      if (!item) return;

      switch (item->kind) {
            case ITEM_FN_DEF:
                  print_typed_head("fn-def", item->type);
                  symbol_print(item->id);
                  if (item->fn_def.type->params) {
                        print_head("fn-params");
                        g_list_foreach(item->fn_def.type->params, (GFunc)pair_print, NULL);
                        print_rparen();
                  }
                  type_print(item->fn_def.type->type);
                  exp_print(item->fn_def.block);
                  break;
            case ITEM_ENUM_DEF:
                  print_typed_head("enum-def", item->type);
                  symbol_print(item->id);
                  print_head("enum-ctor-defs");
                  g_list_foreach(item->enum_def.ctors, (GFunc)pair_print, NULL);
                  print_rparen();
                  break;
            case ITEM_STRUCT_DEF:
                  print_typed_head("struct-def", item->type);
                  symbol_print(item->id);
                  print_head("field-defs");
                  g_list_foreach(item->struct_def.fields, (GFunc)pair_print, NULL);
                  print_rparen();
                  break;
      }
      print_rparen();
}
void item_print_pretty(const struct item* item) {
      if (!item) return;

      switch (item->kind) {
            case ITEM_FN_DEF:
                  printf("fn-def");
                  break;
            case ITEM_ENUM_DEF:
                  printf("enum-def");
                  break;
            case ITEM_STRUCT_DEF:
                  printf("struct-def");
                  break;
      }
}

// *** Statements ***

static void stmt_print(const struct stmt* stmt) {
      if (!stmt) return;

      switch (stmt->kind) {
            case STMT_LET:
                  print_typed_head("let", stmt->type);
                  pat_print(stmt->let.pat);
                  type_print(stmt->let.type);
                  exp_print(stmt->let.exp);
                  print_rparen();
                  break;
            case STMT_RETURN:
                  print_typed_head("return", stmt->type);
                  exp_print(stmt->exp);
                  print_rparen();
                  break;
            case STMT_EXP:
                  print_typed_head("stmt-exp", stmt->type);
                  exp_print(stmt->exp);
                  print_rparen();
                  break;
      }
}

// *** Patterns ***

static void pat_print(const struct pat* pat) {
      if (!pat) return;

      switch (pat->kind) {
            case PAT_WILD:
                  print_head("pat-wild");
                  break;
            case PAT_UNIT:
                  print_head("pat-unit");
                  break;
            case PAT_TRUE:
                  print_head("pat-true");
                  break;
            case PAT_FALSE:
                  print_head("pat-false");
                  break;
            case PAT_STR:
                  print_head("pat-str");
                  break;
            case PAT_U8:
                  print_head("pat-lit");
                  print_leaf("lit-char");
                  break;
            case PAT_I32:
                  print_head("pat-lit");
                  print_leaf("lit-dec");
                  break;

            case PAT_REF:
                  print_head("pat-deref"); // TODO: rename?
                  pat_print(pat->pat);
                  break;

            case PAT_ARRAY:
                  print_head("pat-arr");
                  print_head("pat-arr-elems");
                  g_list_foreach(pat->array.pats, (GFunc)pat_print, NULL);
                  print_rparen();
                  break;
            case PAT_ENUM:
                  print_head("pat-enum");
                  print_head("enum-ctor");
                  symbol_print(pat->ctor.eid);
                  symbol_print(pat->ctor.cid);
                  print_rparen();
                  if (pat->ctor.pats) {
                        print_head("pat-enum-ctor-params");
                        g_list_foreach(pat->ctor.pats, (GFunc)pat_print, NULL);
                        print_rparen();
                  }
                  break;
            case PAT_STRUCT:
                  print_head("pat-struct");
                  symbol_print(pat->strct.id);
                  print_head("pat-fields");
                  g_list_foreach(pat->strct.fields, (GFunc)pair_print, NULL);
                  print_rparen();
                  break;
            case PAT_BIND:
                  if (pat->bind.mut && pat->bind.ref) print_head("pat-ref-mut-id");
                  else if (pat->bind.mut) print_head("pat-mut-id");
                  else if (pat->bind.ref) print_head("pat-ref-id");
                  else print_head("pat-id");
                  symbol_print(pat->bind.id);
                  break;
      }
      print_rparen();
}

// *** Expressions ***

static const char* op_to_str(const char* op, bool unary, bool mut) {
      assert(op);
      if (!strcmp(op, "&")) return mut? "addr-of-mut" : "addr-of";
      if (!strcmp(op, "!")) return "not";
      if (!strcmp(op, "+")) return "add";
      if (!strcmp(op, "-")) return unary? "neg" : "sub";
      if (!strcmp(op, "*")) return unary? "deref" : "mul";
      if (!strcmp(op, "/")) return "div";
      if (!strcmp(op, "%")) return "rem";
      if (!strcmp(op, "=")) return "assign";
      if (!strcmp(op, "+=")) return "assign-add";
      if (!strcmp(op, "-=")) return "assign-sub";
      if (!strcmp(op, "*=")) return "assign-mul";
      if (!strcmp(op, "/=")) return "assign-div";
      if (!strcmp(op, "%=")) return "assign-rem";
      if (!strcmp(op, "&&")) return "and";
      if (!strcmp(op, "||")) return "or";
      if (!strcmp(op, "!=")) return "neq";
      if (!strcmp(op, "==")) return "eq";
      if (!strcmp(op, "<")) return "lt";
      if (!strcmp(op, "<=")) return "leq";
      if (!strcmp(op, ">")) return "gt";
      if (!strcmp(op, ">=")) return "geq";
      assert(false);
}

static void exp_print(const struct exp* exp) {
      if (!exp) return;

      switch (exp->kind) {
            case EXP_UNIT:
                  print_typed_leaf("unit", exp->type);
                  break;
            case EXP_TRUE:
                  print_typed_leaf("true", exp->type);
                  break;
            case EXP_FALSE:
                  print_typed_leaf("false", exp->type);
                  break;
            case EXP_I32:
                  print_typed_leaf("lit-dec", exp->type);
                  break;
            case EXP_U8:
                  print_typed_leaf("lit-char", exp->type);
                  break;
            case EXP_STR:
                  print_typed_leaf("lit-str", exp->type);
                  break;
            case EXP_ID:
                  print_typed_head("id", exp->type);
                  print_leaf(symbol_to_str(exp->id));
                  print_rparen();
                  break;
            case EXP_ENUM:
                  print_typed_head("enum", exp->type);
                  print_head("enum-ctor");
                  symbol_print(exp->lit_enum.eid);
                  symbol_print(exp->lit_enum.cid);
                  print_rparen();
                  if (exp->lit_enum.exps) {
                        print_head("exprs");
                        g_list_foreach(exp->lit_enum.exps, (GFunc)exp_print, NULL);
                        print_rparen();
                  }
                  print_rparen();
                  break;
            case EXP_STRUCT:
                  print_typed_head("struct", exp->type);
                  symbol_print(exp->lit_struct.id);
                  print_head("field-inits");
                  g_list_foreach(exp->lit_struct.fields, (GFunc)pair_print, NULL);
                  print_rparen();
                  print_rparen();
                  break;
            case EXP_ARRAY:
                  print_typed_head("arr", exp->type);
                  print_head("exprs");
                  g_list_foreach(exp->lit_array.exps, (GFunc)exp_print, NULL);
                  print_rparen();
                  print_rparen();
                  break;
            case EXP_LOOKUP:
                  print_typed_head("field-lookup", exp->type);
                  exp_print(exp->lookup.exp);
                  symbol_print(exp->lookup.id);
                  print_rparen();
                  break;
            case EXP_INDEX:
                  print_typed_head("arr-index", exp->type);
                  exp_print(exp->index.exp);
                  exp_print(exp->index.idx);
                  print_rparen();
                  break;
            case EXP_FN_CALL:
                  print_typed_head("fn-call", exp->type);
                  symbol_print(exp->fn_call.id);
                  if (exp->fn_call.exps) {
                        print_head("exprs");
                        g_list_foreach(exp->fn_call.exps, (GFunc)exp_print, NULL);
                        print_rparen();
                  }
                  print_rparen();
                  break;
            case EXP_BOX_NEW:
                  print_typed_head("box-new", exp->type);
                  print_head("exprs");
                  exp_print(exp->exp);
                  print_rparen();
                  print_rparen();
                  break;
            case EXP_MATCH:
                  print_typed_head("match", exp->type);
                  exp_print(exp->match.exp);
                  print_head("match-arms");
                  g_list_foreach(exp->match.arms, (GFunc)pair_print, NULL);
                  print_rparen();
                  print_rparen();
                  break;
            case EXP_IF:
                  print_typed_head("if", exp->type);
                  exp_print(exp->if_else.cond);
                  exp_print(exp->if_else.block_true);
                  exp_print(exp->if_else.block_false);
                  print_rparen();
                  break;
            case EXP_WHILE:
                  print_typed_head("while", exp->type);
                  exp_print(exp->loop_while.cond);
                  exp_print(exp->loop_while.block);
                  print_rparen();
                  break;
            case EXP_LOOP:
                  print_typed_head("loop", exp->type);
                  exp_print(exp->exp);
                  print_rparen();
                  break;
            case EXP_BLOCK:
                  print_typed_head("block", exp->type);
                  g_list_foreach(exp->block.stmts, (GFunc)stmt_print, NULL);
                  exp_print(exp->block.exp);
                  print_rparen();
                  break;
            case EXP_UNARY:
                  print_typed_head(op_to_str(exp->unary.op, true, exp->unary.mut), exp->type);
                  exp_print(exp->unary.exp);
                  print_rparen();
                  break;
            case EXP_BINARY:
                  print_typed_head(op_to_str(exp->binary.op, false, false), exp->type);
                  exp_print(exp->binary.left);
                  exp_print(exp->binary.right);
                  print_rparen();
                  break;
      }
}



// *** Pairs ***

static void pair_print(const struct pair* pair) {
      if (!pair) return;

      switch (pair->kind) {
            case PAIR_FIELD_DEF:
                  print_head("field-def");
                  symbol_print(pair->field_def.id);
                  type_print(pair->field_def.type);
                  print_rparen();
                  break;
            case PAIR_CTOR_DEF:
                  print_head("enum-ctor-def");
                  symbol_print(pair->ctor_def.id);
                  if (pair->ctor_def.types) {
                        print_head("enum-ctor-params");
                        g_list_foreach(pair->ctor_def.types, (GFunc)type_print, NULL);
                        print_rparen();
                  }
                  print_rparen();
                  break;
            case PAIR_PARAM:
                  print_head("fn-param");
                  pat_print(pair->param.pat);
                  type_print(pair->param.type);
                  print_rparen();
                  break;
            case PAIR_FIELD_PAT:
                  print_head("pat-field");
                  symbol_print(pair->field_pat.id);
                  pat_print(pair->field_pat.pat);
                  print_rparen();
                  break;
            case PAIR_FIELD_INIT:
                  print_head("field-init");
                  symbol_print(pair->field_init.id);
                  exp_print(pair->field_init.exp);
                  print_rparen();
                  break;
            case PAIR_MATCH_ARM:
                  print_head("match-arm");
                  print_head("pats");
                  g_list_foreach(pair->match_arm.pats, (GFunc)pat_print, NULL);
                  print_rparen();
                  exp_print(pair->match_arm.block);
                  print_rparen();
                  break;
      }
}

// *** Types ***

static void type_print(const struct type* type) {
      if (!type) return;

      if (type->kind == TYPE_ERROR
            || type->kind == TYPE_OK
            || type->kind == TYPE_DIV)
            return;

      switch (type->kind) {
            case TYPE_UNIT:
                  print_leaf("type-unit");
                  break;
            case TYPE_I32:
                  print_leaf("type-i32");
                  break;
            case TYPE_U8:
                  print_leaf("type-u8");
                  break;
            case TYPE_BOOL:
                  print_leaf("type-bool");
                  break;
            case TYPE_REF:
                  print_head("type-ref");
                  type_print(type->type);
                  print_rparen();
                  break;
            case TYPE_MUT:
                  print_head("type-mut");
                  type_print(type->type);
                  print_rparen();
                  break;
            case TYPE_SLICE:
                  print_head("type-arr");
                  type_print(type->type);
                  print_rparen();
                  break;
            case TYPE_ARRAY:
                  print_head("type-arr");
                  type_print(type->type);
                  print_leaf("lit-dec");
                  print_rparen();
                  break;
            case TYPE_BOX:
                  print_head("type-box");
                  type_print(type->type);
                  print_rparen();
                  break;
            case TYPE_ID:
                  symbol_print(type->id);
                  break;
      }
}

// TODO
void type_print_pretty(const struct type* type) {
      if (!type) return;

      switch (type->kind) {
            case TYPE_ERROR:
                  printf("ERROR!");
                  break;
            case TYPE_OK:
                  printf("ok!");
                  break;
            case TYPE_DIV:
                  printf("!");
                  break;
            case TYPE_UNIT:
                  printf("()");
                  break;
            case TYPE_I32:
                  printf("i32");
                  break;
            case TYPE_U8:
                  printf("u8");
                  break;
            case TYPE_BOOL:
                  printf("bool");
                  break;
            case TYPE_REF:
                  printf("&");
                  type_print_pretty(type->type);
                  break;
            case TYPE_MUT:
                  printf("mut ");
                  type_print_pretty(type->type);
                  break;
            case TYPE_SLICE:
                  printf("[");
                  type_print_pretty(type->type);
                  printf("]");
                  break;
            case TYPE_ARRAY:
                  printf("[");
                  type_print_pretty(type->type);
                  printf(";%d]", type->length);
                  break;
            case TYPE_BOX:
                  printf("Box<");
                  type_print_pretty(type->type);
                  printf(">");
                  break;
            case TYPE_FN:
                  // TODO
                  printf("fn (TODO) -> TODO");
                  break;
            case TYPE_ID:
                  printf("%s", symbol_to_str(type->id));
                  break;
      }
}

/* LLVM */

void llvm_crate(const GList* items){
  printf("@.str = private unnamed_addr constant [3 x i8] c\"%%s\\00\", align 1\n");
  printf("@.str1 = private unnamed_addr constant [3 x i8] c\"%%d\\00\", align 1\n");
  
  g_list_foreach((GList*)items, (GFunc)llvm_strings, NULL);
  printf("\n");
  g_list_foreach((GList*)items, (GFunc)llvm_item, NULL);
  
  printf("; Function Attrs: nounwind\ndeclare i32 @printf(i8*, ...) #0\n\n");
  printf("!0 = !{!\"clang version 3.6.0 (tags/RELEASE_360/final)\"}\n");
}

void llvm_item(const struct item* item){
  last_register = 0;
  last_label = 0;
  last_if = 0;
  GList* p;
  last_args = NULL;
  switch (item->kind){
    case ITEM_FN_DEF:{
      last_args = item->fn_def.type->params;
        
      // NoUnwind
      printf("; Function Attrs: nounwind\n");
      
      // Print function name
      printf("define %s @%s(", llvm_get_type(item->fn_def.type->type), symbol_to_str(item->id));
      last_type = item->fn_def.type->type;
      
      // Loop through all params
      for(p = item->fn_def.type->params; p; p = p->next){
        struct pair* param = p->data;
        
        printf("%s %%%s", llvm_get_type(param->param.type), symbol_to_str(param->param.pat->bind.id));  // TODO (POSSIBLY) : Make sure there is no clash between ids and other registers. i.e.: variables called "r" or "retval" might cause problems
        
        if (p->next)
          printf(", ");
      }
      
      printf(") #0 {\n");
      printf("entry:\n");
      
      // Loop through all params
      for(p = item->fn_def.type->params; p; p = p->next){
        struct pair* param = p->data;
        
        printf("  %%%s.addr = alloca %s, align 4\n", symbol_to_str(param->param.pat->bind.id), llvm_get_type(param->param.type));
        printf("  store %s %%%s, %s* %%%s.addr, align 4\n", llvm_get_type(param->param.type), symbol_to_str(param->param.pat->bind.id), llvm_get_type(param->param.type), symbol_to_str(param->param.pat->bind.id));
        
      }
      
      //printf("%retval = alloca i32, align 4");  // ???: There isn't a retval register for every function

      llvm_exp(item->fn_def.block);
      
      // If main
      if (!strcmp(symbol_to_str(item->id), "main"))
        printf("  ret i32 0\n");
      
      printf("}\n\n");
      break;
    }

    case ITEM_ENUM_DEF:
      break;
    case ITEM_STRUCT_DEF:
      //item->
      printf("%%struct.%s = type { ", symbol_to_str(item->id));
      for(p = item->struct_def.fields; p; p = p->next){
        struct pair* field_def = p->data; 

        //print statement

        printf("%s", llvm_get_type(field_def->field_def.type));

        if(p->next){
          printf(", ");
        }
      }
      printf(" }\n\n");
      break;
  }
  
}

const char* llvm_op_to_str(const char* op){

  //if (!strcmp(op, "&")) return mut? "addr-of-mut" : "addr-of";
  if (!strcmp(op, "!")) return "not";
  
  // MATH
  if (!strcmp(op, "+")) return "add";
  if (!strcmp(op, "-")) return "sub";
  if (!strcmp(op, "*")) return "mul";
  if (!strcmp(op, "/")) return "sdiv";
  if (!strcmp(op, "%")) return "srem";
  
  // ASSIGN
  if (!strcmp(op, "=")) return "assign";
  if (!strcmp(op, "+=")) return "add";
  if (!strcmp(op, "-=")) return "sub";
  if (!strcmp(op, "*=")) return "mul";
  if (!strcmp(op, "/=")) return "sdiv";
  if (!strcmp(op, "%=")) return "srem";
  
  // BOOLEAN
  if (!strcmp(op, "&&")) return "and"; // TODO
  if (!strcmp(op, "||")) return "or";  // TODO
  if (!strcmp(op, "!=")) return "ne";
  if (!strcmp(op, "==")) return "eq";
  if (!strcmp(op, "<")) return "slt";
  if (!strcmp(op, "<=")) return "sle";
  if (!strcmp(op, ">")) return "sgt";
  if (!strcmp(op, ">=")) return "sge";
}

const char* llvm_get_type(const struct type* type){
  
  switch (type->kind){
    case TYPE_INVALID:
      return "inv";
      break;
    case TYPE_ERROR:
      return "err";
      break;
    case TYPE_OK:
      return "ok";
      break;
    case TYPE_UNIT:
      return "i32";
    case TYPE_I32:
      return "i32";
    case TYPE_U8:
      return "i8";
    case TYPE_BOOL:
      return "bool";
    case TYPE_DIV:
      return "div";
      break;
    case TYPE_ID:
      return "\%struct.";
      break;
    case TYPE_REF:
      return "ref";
      break;
    case TYPE_MUT:
      return llvm_get_type(type->type);
      break;
    case TYPE_SLICE:
      return "slice";
      break;
    case TYPE_ARRAY:
      return "[]";
      break;
    case TYPE_BOX:
      return "<>";
      break;
    case TYPE_FN:
      return "fn";
      break;
  }
  
  return "<TYPE>";
}

void llvm_print_type(const struct type* type){
  printf("%s", llvm_get_type(type));
}

void llvm_exp(const struct exp* exp){
  int l;
  GList* p; 
  if (!exp) return;
  last_register++;
  char* function_var = "";
  
  switch (exp->kind) {
    case EXP_UNIT:
      //printf("%%r%d = <UNIT>\n", last_register);
      return;
      break;
    case EXP_TRUE:
      printf("%%r%d = <TRUE>\n", last_register);
      return;
      break;
    case EXP_FALSE:
      printf("%%r%d = <FALSE>\n", last_register);
      return;
      break;
    case EXP_I32:
      printf("%%r%d = <I32>\n", last_register);
      return;
      break;
    case EXP_U8:
      printf("%%r%d = <U8>\n", last_register);
      return;
      break;
    case EXP_STR:
      printf("%%r%d = <STR>\n", last_register);
      return;
      break;
    case EXP_ID:
    
      // Check if variable is function variable 
      for(p = last_args; p; p = p->next){
        struct pair* param = p->data;
        if (!strcmp(symbol_to_str(param->param.pat->bind.id),symbol_to_str(exp->id))){
          function_var = ".addr";
          break;
        }
      }
      printf("  %%r%d = load %s* %%%s%s, align 4\n", last_register, llvm_get_type(exp->type), symbol_to_str(exp->id), function_var);

    
      return;
      break;
    case EXP_ENUM:
      printf("%%r%d = <ENUM>\n", last_register);
      return;
      break;
    case EXP_STRUCT:
      last_register++;
      //print struct variable name, %struct.Name
      //printf("%%struct%d = getelementptr inbounds %%struct.%s* %%s, i32 0, i32 0\n", "struct_mem", "struct_name", "struct_var");
      //printf("store i32 %d, i32* %s%d, align 4\n", 10, "struct_mem", last_register);

	return; 
	break;
    case EXP_ARRAY:
      printf("%%r%d = <ARRAY>\n", last_register);
      return;
      break;
    case EXP_LOOKUP:
      printf("%%r%d = <LOOKUP>\n", last_register);
      return;
      break;
    case EXP_INDEX:
      printf("%%r%d = <INDEX>\n", last_register);
      return;
      break;

    case EXP_FN_CALL: 

      if (!strcmp(symbol_to_str(exp->fn_call.id), "printi")){
      //get parameter list and type
      for(p = exp->fn_call.exps; p; p = p->next){
            struct exp* expression = p->data;

	    if(expression->kind == EXP_I32){
		num_to_print = expression->num; 
	}else{
 
            llvm_exp(expression);
	}        
      }
            
	if(num_to_print == 0){

      printf("  %%r%d = call i32 (i8*, ...)* @printi(i8* getelementptr inbounds ([3 x i8]* @.str, i32 0, i32 0), i32 %%r%d) #1\n", last_register);
	}else if(num_to_print != 0){
	printf("  %%r%d = call i32 (i8*, ...)* @printi(i8* getelementptr inbounds ([3 x i8]* @.str, i32 0, i32 0), i32 %d) #1\n", last_register, num_to_print);

	}

      }else if(!strcmp(symbol_to_str(exp->fn_call.id), "prints")){
		for(p = exp->fn_call.exps; p; p = p->next){
			struct exp* expression = p->data; 
			int len = strlen(expression->str);
			//printf("%d\n", len);
			
			printf("  %%r%d = call i32 (i8*, ...)* @prints(i8* getelementptr inbounds ([%d x i8]* @.str%d, i32 0, i32 0)) #1\n", last_register,len+1,last_string);
			last_string++;

		}
	}else{

            printf("  %%r%d = call %s @%s(", last_register, llvm_get_type(exp->type), symbol_to_str(exp->fn_call.id));
            for(p = exp->fn_call.exps; p; p = p->next){
            struct exp* expression = p->data; 
      
                    printf("%s %d", llvm_get_type(expression->type), expression->num); 

            if(p->next){
            printf(", ");
            }     
                }
      }   
      return;
      break;
    case EXP_BOX_NEW:
      printf("%%r%d = <BOX NEW>\n", last_register);
      return;
      break;
    case EXP_MATCH:
      printf("%%r%d = <MATCH>\n", last_register);
      return;
      break;
    case EXP_IF:
      l = last_label++;
      last_if = l;
      llvm_exp(exp->if_else.cond);
      printf("  br i1 %%cmp%d, label %%if.then%d, label %%if.else%d\n\nif.then%d:\n",
             last_register,
             l,
             l,
             l);
      
      llvm_exp(exp->if_else.block_true);
      printf("  br label %%if.end%d\n\nif.else%d:\n",
             l,
             l);
    
      llvm_exp(exp->if_else.block_false);
      printf("  br label %%if.end%d\n\nif.end%d:\n",
             l,
             l);
    
      return;
      break;
    case EXP_WHILE:
      l = last_label++;
      
      printf("  br label %%while.cond%d\n\nwhile.cond%d:\n",
             l,
             l);
    
      llvm_exp(exp->loop_while.cond);
      printf("  br i1 %%cmp%d, label %%while.body%d, label %%while.end%d\n\n",
             last_register,
             l,
             l);
      
      printf("while.body%d:\n", l);
      llvm_exp(exp->loop_while.block);
      
      printf("  br label %%while.cond%d\n\nwhile.end%d:\n", l, l);
      return;
      break;
    case EXP_LOOP:
      l = last_label++;
      printf("  br label %%loop.begin%d\n\nloop.begin%d:\n", l, l);
      llvm_exp(exp->exp);
      printf("  br label %%loop.begin%d\n\nloop.begin%d:\n", l, l);
      return;
      break;
    case EXP_BLOCK:
      g_list_foreach(exp->block.stmts, (GFunc)llvm_stmt, NULL);
      llvm_exp(exp->block.exp);
      return;
      break;
    case EXP_UNARY:
      llvm_exp(exp->unary.exp);
      return;
      break;
    case EXP_BINARY:
    
      // Plain assignment
      if (!strcmp(exp->binary.op, "=")){
        // Check if variable is function variable 
        for(p = last_args; p; p = p->next){
          struct pair* param = p->data;
          if (!strcmp(symbol_to_str(param->param.pat->bind.id),symbol_to_str(exp->binary.left->id))){
            function_var = ".addr";
            break;
          }
        }
        // Register
        if (exp->binary.right->kind != EXP_I32){
          llvm_exp(exp->binary.right);

          printf("  store %s %%r%d, %s* %%%s%s, align 4\n", llvm_get_type(exp->binary.right->type), last_register, llvm_get_type(exp->binary.right->type), symbol_to_str(exp->binary.left->id), function_var);   
        }
        // Plain number
        else{
          printf("  store i32 %d, i32* %%%s%s, align 4\n", exp->binary.right->num, symbol_to_str(exp->binary.left->id), function_var);   

        }
        // TODO: Add string support here
        // store i8* getelementptr inbounds ([5 x i8]* @.str, i32 0, i32 0), i8** %x, align 4
        // template:
        // store i8* getelementptr inbounds ([<length> x i8]* <string reference>, i32 0, i32 0), i8** %<var id>, align 4
        
      }
      // Combo assignment
      else if (!strcmp(exp->binary.op, "+=") ||
          !strcmp(exp->binary.op, "-=") ||
          !strcmp(exp->binary.op, "*=") ||
          !strcmp(exp->binary.op, "/=") ||
          !strcmp(exp->binary.op, "%="))  {
        // Check if variable is function variable 
        for(p = last_args; p; p = p->next){
          struct pair* param = p->data;
          if (!strcmp(symbol_to_str(param->param.pat->bind.id),symbol_to_str(exp->binary.left->id))){
            function_var = ".addr";
            break;
          }
        }
        
        llvm_exp(exp->binary.left);
        l = last_register;
        
        if (exp->binary.right->kind != EXP_I32){
          llvm_exp(exp->binary.right);
        }
        last_register++;

        // Print beginning
        printf("  %%r%d = %s i32 %%r%d, ", last_register, llvm_op_to_str(exp->binary.op), l);

        // Print end
        if (exp->binary.right->kind == EXP_I32)
          printf("%d", exp->binary.right->num);
        else
          printf("%%r%d", last_register - 1);


        printf("\n  store i32 %%r%d, i32* %%%s%s, align 4\n", last_register, symbol_to_str(exp->binary.left->id), function_var);

        
      } 
      // Arithmetic
      else if (!strcmp(exp->binary.op, "+") ||
               !strcmp(exp->binary.op, "-") ||
               !strcmp(exp->binary.op, "*") ||
               !strcmp(exp->binary.op, "/") ||
               !strcmp(exp->binary.op, "%"))  {
        
        // Do left expression
        if (exp->binary.left->kind != EXP_I32){
          llvm_exp(exp->binary.left);
          l = last_register;
        }

        // Do right expression
        if (exp->binary.right->kind != EXP_I32){
          llvm_exp(exp->binary.right);
          last_register++;
        }

        // Print beginning
        printf("  %%r%d = %s i32 ", last_register, llvm_op_to_str(exp->binary.op));

        // Print left
        if (exp->binary.left->kind == EXP_I32)
          printf("%d, ", exp->binary.left->num);
        else
          printf("%%r%d, ", l);

        // Print right
        if (exp->binary.right->kind == EXP_I32)
          printf("%d", exp->binary.right->num);
        else
          printf("%%r%d", last_register - 1); 
        printf("\n");
      }
      // AND
      else if(!strcmp(exp->binary.op, "&&")){

        llvm_exp(exp->binary.left);
        printf("  br i1 %%cmp%d, label %%land.lhs.true%d, label %%if.end%d\n\n", last_register, last_label, last_if);
        printf("land.lhs.true%d:\n", last_label++);
        llvm_exp(exp->binary.right);
      }
      else if(!strcmp(exp->binary.op, "||")){
        llvm_exp(exp->binary.left);
        printf("  br i1 %%cmp%d, label %%if.then%d, label %%lor.lhs.false%d\n\n", last_register, last_if, last_label);
        printf("lor.lhs.false%d:\n", last_label++);
        llvm_exp(exp->binary.right);
      }
      // Boolean
      // TODO: Possibly merge with arith
      else{
        // %0 = load i32* %x, align 4  llvm_exp(left)
        // %cmp = icmp sgt i32 %0, 2   
        
        // Do left expression
        if (exp->binary.left->kind != EXP_I32){
          llvm_exp(exp->binary.left);
          l = last_register;
        }

        // Do right expression
        if (exp->binary.right->kind != EXP_I32){
          llvm_exp(exp->binary.right);
          last_register++;
        }
        
        // Print beginning
        printf("  %%cmp%d = icmp %s %s ", last_register, llvm_op_to_str(exp->binary.op), llvm_get_type(exp->binary.left->type));
        
        // Print left
        if (exp->binary.left->kind == EXP_I32)
          printf("%d, ", exp->binary.left->num);
        else
          printf("%%r%d, ", l);
        
        // Print right
        if (exp->binary.right->kind == EXP_I32)
          printf("%d", exp->binary.right->num);
        else
          printf("%%r%d", last_register - 1); 
        printf("\n");
        
      }
      return;
  }
  
  printf("%%r%d = ...\n", last_register);
  
}

static void llvm_stmt(const struct stmt* stmt){
  last_register++;
  struct type* t;
  
  switch (stmt->kind) {
    case STMT_LET:
      if (stmt->let.type)
        t = stmt->let.type;
      else
        t = stmt->let.exp->type;
      
        printf("  %%%s = alloca %s, align 4\n", symbol_to_str(stmt->let.pat->bind.id), llvm_get_type(t));
      
        
      if (stmt->let.exp){
        
        llvm_exp(stmt->let.exp);
        printf("  store %s %%r%d, %s* %%%s, align 4\n", llvm_get_type(t), last_register, llvm_get_type(t), symbol_to_str(stmt->let.pat->bind.id));
        
      }
      
      break;
    case STMT_RETURN:
      llvm_exp(stmt->exp);
      printf("  ret %s %%%d\n", llvm_get_type(last_type), last_register);
      break;
    case STMT_EXP:
      llvm_exp(stmt->exp);
      break;
  }
  
}

static void llvm_strings(const struct item* item){
  int i = 2;
  struct stmt* stmt;
  struct exp* exp;
  GList* l;
  
  if (item->kind == ITEM_FN_DEF){
    
    for(l = item->fn_def.block->block.stmts; l; l = l->next){
      stmt = l->data;
      
      if (stmt->kind == STMT_EXP){
        if (stmt->exp->kind == EXP_FN_CALL){
          if(!strcmp(symbol_to_str(stmt->exp->fn_call.id),"prints")){
            exp = stmt->exp->fn_call.exps->data;
            if (exp->kind == EXP_STR){
              //@.str = private unnamed_addr constant [3 x i8] c"%s\00", align 1
              printf("@.str%d = private unnamed_addr constant [%u x i8] c\"%s\\00\", align 1\n",
                     i,
                     (unsigned)strlen(exp->str) + 1,
                     exp->str);
              i++;
            }
          }
        }
      }
      
    }
    /*
    if (exp->block.exp && exp->block.exp->kind == EXP_FN_CALL){
      if(!strcmp(symbol_to_str(exp->block.exp->fn_call.id),"prints")){
        exp = exp->block.exp->fn_call.exps->data;
        if (exp->kind == EXP_STR){
          //@.str = private unnamed_addr constant [3 x i8] c"%s\00", align 1
          printf("@.str%d = private unnamed_addr constant [%u x i8] c\"%s\\00\", align 1\n",
                 i,
                 (unsigned)strlen(exp->str) + 1,
                 exp->str);
        }
      }
    }*/
     
  }

}
  
