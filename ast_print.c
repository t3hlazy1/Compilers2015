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
                  llvm_binary(exp, 0);
                  break;
                  print_typed_head(op_to_str(exp->binary.op, false, false), exp->type);
                  //exp_print(exp->binary.left);
                  //exp_print(exp->binary.right);
                  print_rparen();
                    
                  if (exp->binary.left->kind == EXP_I32)
                    printf("\nmov eax, %d\n", exp->binary.left->num);
                  if (exp->binary.left->kind == EXP_I32)
                    printf("\nmov edx, %d\n", exp->binary.right->num);
                    
                  exp_print(exp->binary.left);
                  exp_print(exp->binary.right);
                  printf("\n");
                  if (!strcmp(exp->binary.op, "+")) printf("add");
                  if (!strcmp(exp->binary.op, "-")) printf("sub");
                  if (!strcmp(exp->binary.op, "*")) printf("imul");
                  if (!strcmp(exp->binary.op, "/")) printf("idiv");
                  printf(" eax, edx\n");
        
                  // DO LEFT -> SETS eax to its value
                  // DO RIGHT -> SETS edx to its value
                  // op eax, edx
                    
                  // mov eax, left
                  // mov edx, right
                  // op eax, edx
                    
                    
                    
                  
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
  g_list_foreach((GList*)items, (GFunc)llvm_item, NULL);
}

void llvm_item(const struct item* item){
  GList* p;
  switch (item->kind){
    case ITEM_FN_DEF:{
      
      // NoUnwind
      printf("; Function Attrs: nounwind\n");
      
      // Print function name
      printf("define %s @%s(", llvm_get_type(item->type), symbol_to_str(item->id));
      
      // Loop through all params
      for(p = item->fn_def.type->params; p; p = p->next){
        struct pair* param = p->data;
        
        printf("%s %%%s", llvm_get_type(param->param.type), "PUT PARAM ID HERE"); // FIX
        
        if (p->next)
          printf(", ");
      }
      
      printf(") #0 {\n");
      printf("entry:\n");
      
      llvm_exp(item->fn_def.block);
      
      printf("}\n\n");
      break;
    }

    case ITEM_ENUM_DEF:
      break;
    case ITEM_STRUCT_DEF:
      break;
  }
  
}

const char* llvm_get_type(const struct type* type){
  return "<type>";
  
  switch (type->kind){
    case TYPE_INVALID:
    case TYPE_ERROR:
    case TYPE_OK:
    case TYPE_UNIT:
    case TYPE_I32:
      printf("i32");
      break;
    case TYPE_U8:
      printf("i8"); // ??
      break;
    case TYPE_BOOL:
      printf("bool");
      break;
    case TYPE_DIV:
    case TYPE_ID:
    case TYPE_REF:
    case TYPE_MUT:
    case TYPE_SLICE:
    case TYPE_ARRAY:
    case TYPE_BOX:
    case TYPE_FN:
      break;
  }
}

void llvm_print_type(const struct type* type){
  printf("%s", llvm_get_type(type));
}

void llvm_exp(const struct exp* exp){
  if (!exp) return;
  
  switch (exp->kind) {
    case EXP_UNIT:
    case EXP_TRUE:
    case EXP_FALSE:
    case EXP_I32:
    case EXP_U8:
    case EXP_STR:
    case EXP_ID:
    case EXP_ENUM:
    case EXP_STRUCT:
    case EXP_ARRAY:
    case EXP_LOOKUP:
    case EXP_INDEX:
    case EXP_FN_CALL:
    case EXP_BOX_NEW:
    case EXP_MATCH:
    case EXP_IF:
    case EXP_WHILE:
    case EXP_LOOP:
    case EXP_BLOCK:
      g_list_foreach(exp->block.stmts, (GFunc)llvm_stmt, NULL);
      llvm_exp(exp->block.exp);
      break;
    case EXP_UNARY:
    case EXP_BINARY:
      llvm_binary(exp, 0);
      break;
  }
  
}

// LEG: 0 = left
//      1 = right
static void llvm_binary(const struct exp* exp, int leg){
  
  char* r0 = "eax";
  char* r1 = "edx";
  if (leg == 1){
    r0 = r1;
    r1 = "ecx";
  }
  
  if (exp->binary.left->kind == EXP_I32)
    printf("  mov %s, %d\n", r0, exp->binary.left->num);
  else
    llvm_binary(exp->binary.left, 0);
    
  if (exp->binary.right->kind == EXP_I32)
    printf("  mov %s, %d\n", r1, exp->binary.right->num);
  else
    llvm_binary(exp->binary.right, 1);
  
  printf("  ");
  if (!strcmp(exp->binary.op, "+")) printf("add");
  if (!strcmp(exp->binary.op, "-")) printf("sub");
  if (!strcmp(exp->binary.op, "*")) printf("imul");
  if (!strcmp(exp->binary.op, "/")) printf("idiv");
  printf(" %s, %s\n", r0, r1);
  
}

static void llvm_stmt(const struct stmt* stmt){

  switch (stmt->kind) {
    case STMT_LET:
      llvm_exp(stmt->let.exp);
      break;
    case STMT_RETURN:
      llvm_exp(stmt->exp);
      break;
    case STMT_EXP:
      llvm_exp(stmt->exp);
      break;
  }
  
}