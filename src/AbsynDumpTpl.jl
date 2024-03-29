module AbsynDumpTpl

using MetaModelica
using ExportAll

import Main.Tpl

import Absyn

import .Main.AbsynUtil

import .Main.Util

import .Main.Config

import .Main.Dump

import .Main.System

import .Main.Flags

function lm_9(
  in_txt::Tpl.Text,
  in_items::List{<:Absyn.Class},
  in_a_options::Dump.DumpOptions,
)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Absyn.Class}
    local a_options::Dump.DumpOptions
    local i_cls::Absyn.Class
    @match (in_txt, in_items, in_a_options) begin
      (txt, nil(), _) => begin
        txt
      end

      (txt, i_cls <| rest, a_options) => begin
        @assign txt = dumpClass(txt, i_cls, a_options)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_9(txt, rest, a_options)
        txt
      end
    end
  end
  return out_txt
end

function dump(
  in_txt::Tpl.Text,
  in_a_program::Absyn.Program,
  in_a_options::Dump.DumpOptions,
)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local a_options::Dump.DumpOptions
    local i_classes::List{Absyn.Class}
    local i_within__::Absyn.Within
    local l_cls__str::Tpl.Text
    local l_within__str::Tpl.Text
    @match (in_txt, in_a_program, in_a_options) begin
      (txt, Absyn.PROGRAM(classes = nil()), _) => begin
        txt
      end

      (txt, Absyn.PROGRAM(within_ = i_within__, classes = i_classes), a_options) => begin
        @assign l_within__str = dumpWithin(Tpl.emptyTxt, i_within__)
        @assign l_cls__str = Tpl.pushIter(
          Tpl.emptyTxt,
          Tpl.ITER_OPTIONS(
            0,
            NONE(),
            SOME(Tpl.ST_STRING_LIST(list(";\\n", "\\n"), true)),
            0,
            0,
            Tpl.ST_NEW_LINE(),
            0,
            Tpl.ST_NEW_LINE(),
          ),
        )
        @assign l_cls__str = lm_9(l_cls__str, i_classes, a_options)
        @assign l_cls__str = Tpl.popIter(l_cls__str)
        @assign txt = Tpl.writeText(txt, l_within__str)
        @assign txt = Tpl.writeText(txt, l_cls__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(";"))
        txt
      end

      (txt, _, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function dumpClass(txt::Tpl.Text, a_cls::Absyn.Class, a_options::Dump.DumpOptions)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = dumpClassElement(txt, a_cls, "", "", "", "", a_options)
  return out_txt
end

function dumpWithin(in_txt::Tpl.Text, in_a_within::Absyn.Within)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_path::Absyn.Path
    local l_path__str::Tpl.Text
    @match (in_txt, in_a_within) begin
      (txt, Absyn.TOP(__)) => begin
        txt
      end

      (txt, Absyn.WITHIN(path = i_path)) => begin
        @assign l_path__str = dumpPath(Tpl.emptyTxt, i_path)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("within "))
        @assign txt = Tpl.writeText(txt, l_path__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING_LIST(list(";\\n", "\\n"), true))
        txt
      end

      (txt, _) => begin
        Tpl.addSourceTemplateError(
          "Unknown operation",
          Tpl.sourceInfo("AbsynDumpTpl.tpl", 29, 56),
        )
        txt
      end
    end
  end
  return out_txt
end

function dumpClassHeader(
  in_txt::Tpl.Text,
  in_a_cls::Absyn.Class,
  in_a_final__str::String,
  in_a_redecl__str::String,
  in_a_repl__str::String,
  in_a_io__str::String,
)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local a_final__str::String
    local a_redecl__str::String
    local a_repl__str::String
    local a_io__str::String
    local i_cls::Absyn.Class
    local i_restriction::Absyn.Restriction
    local l_pref__str::Tpl.Text
    local l_res__str::Tpl.Text
    @match (
      in_txt,
      in_a_cls,
      in_a_final__str,
      in_a_redecl__str,
      in_a_repl__str,
      in_a_io__str,
    ) begin
      (
        txt,
        i_cls && Absyn.CLASS(restriction = i_restriction),
        a_final__str,
        a_redecl__str,
        a_repl__str,
        a_io__str,
      ) => begin
        @assign l_res__str = dumpRestriction(Tpl.emptyTxt, i_restriction)
        @assign l_pref__str = dumpClassPrefixes(
          Tpl.emptyTxt,
          i_cls,
          a_final__str,
          a_redecl__str,
          a_repl__str,
          a_io__str,
        )
        @assign txt = Tpl.writeText(txt, l_pref__str)
        @assign txt = Tpl.writeText(txt, l_res__str)
        txt
      end

      (txt, _, _, _, _, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function dumpClassElement(
  in_txt::Tpl.Text,
  in_a_cls::Absyn.Class,
  in_a_final__str::String,
  in_a_redecl__str::String,
  in_a_repl__str::String,
  in_a_io__str::String,
  in_a_options::Dump.DumpOptions,
)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local a_final__str::String
    local a_redecl__str::String
    local a_repl__str::String
    local a_io__str::String
    local a_options::Dump.DumpOptions
    local i_name::Absyn.Ident
    local i_body::Absyn.ClassDef
    local i_cls::Absyn.Class
    local l_body__str::Tpl.Text
    local l_header__str::Tpl.Text
    @match (
      in_txt,
      in_a_cls,
      in_a_final__str,
      in_a_redecl__str,
      in_a_repl__str,
      in_a_io__str,
      in_a_options,
    ) begin
      (
        txt,
        i_cls && Absyn.CLASS(body = i_body, name = i_name),
        a_final__str,
        a_redecl__str,
        a_repl__str,
        a_io__str,
        a_options,
      ) => begin
        @assign l_header__str = dumpClassHeader(
          Tpl.emptyTxt,
          i_cls,
          a_final__str,
          a_redecl__str,
          a_repl__str,
          a_io__str,
        )
        @assign l_body__str = dumpClassDef(Tpl.emptyTxt, i_body, i_name, a_options)
        @assign txt = Tpl.writeText(txt, l_header__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(" "))
        @assign txt = Tpl.writeText(txt, l_body__str)
        txt
      end

      (txt, _, _, _, _, _, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function lm_15(in_txt::Tpl.Text, in_items::List{<:String})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{String}
    local i_typevar::String
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_typevar <| rest) => begin
        @assign txt = Tpl.writeStr(txt, i_typevar)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_15(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function fun_16(in_txt::Tpl.Text, in_a_typeVars::List{<:String})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_typeVars::List{String}
    @match (in_txt, in_a_typeVars) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_typeVars) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("<"))
        @assign txt = Tpl.pushIter(
          txt,
          Tpl.ITER_OPTIONS(
            0,
            NONE(),
            SOME(Tpl.ST_STRING(", ")),
            0,
            0,
            Tpl.ST_NEW_LINE(),
            0,
            Tpl.ST_NEW_LINE(),
          ),
        )
        @assign txt = lm_15(txt, i_typeVars)
        @assign txt = Tpl.popIter(txt)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(">"))
        txt
      end
    end
  end
  return out_txt
end

function lm_17(in_txt::Tpl.Text, in_items::List{<:Absyn.Annotation})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Absyn.Annotation}
    local i_a::Absyn.Annotation
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_a <| rest) => begin
        @assign txt = dumpAnnotation(txt, i_a)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_17(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function lm_18(
  in_txt::Tpl.Text,
  in_items::List{<:Absyn.ClassPart},
  in_a_options::Dump.DumpOptions,
)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Absyn.ClassPart}
    local a_options::Dump.DumpOptions
    local x_idx::Integer
    local i_class__part::Absyn.ClassPart
    @match (in_txt, in_items, in_a_options) begin
      (txt, nil(), _) => begin
        txt
      end

      (txt, i_class__part <| rest, a_options) => begin
        @assign x_idx = Tpl.getIteri_i0(txt)
        @assign txt = dumpClassPart(txt, i_class__part, x_idx, a_options)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_18(txt, rest, a_options)
        txt
      end
    end
  end
  return out_txt
end

function fun_19(in_txt::Tpl.Text, in_a_ann__str::Tpl.Text)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_ann__str::Tpl.Text
    @match (in_txt, in_a_ann__str) begin
      (txt, Tpl.MEM_TEXT(tokens = nil())) => begin
        txt
      end

      (txt, i_ann__str) => begin
        @assign txt = Tpl.writeText(txt, i_ann__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(";"))
        txt
      end
    end
  end
  return out_txt
end

function lm_20(in_txt::Tpl.Text, in_items::List{<:Absyn.ElementArg})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Absyn.ElementArg}
    local i_arg::Absyn.ElementArg
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_arg <| rest) => begin
        @assign txt = dumpElementArg(txt, i_arg)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_20(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function fun_21(in_txt::Tpl.Text, in_a_arguments::List{<:Absyn.ElementArg})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_arguments::List{Absyn.ElementArg}
    @match (in_txt, in_a_arguments) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_arguments) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("("))
        @assign txt = Tpl.pushIter(
          txt,
          Tpl.ITER_OPTIONS(
            0,
            NONE(),
            SOME(Tpl.ST_STRING(", ")),
            0,
            0,
            Tpl.ST_NEW_LINE(),
            0,
            Tpl.ST_NEW_LINE(),
          ),
        )
        @assign txt = lm_20(txt, i_arguments)
        @assign txt = Tpl.popIter(txt)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(")"))
        txt
      end
    end
  end
  return out_txt
end

function lm_22(
  in_txt::Tpl.Text,
  in_items::List{<:Absyn.ClassPart},
  in_a_options::Dump.DumpOptions,
)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Absyn.ClassPart}
    local a_options::Dump.DumpOptions
    local x_idx::Integer
    local i_class__part::Absyn.ClassPart
    @match (in_txt, in_items, in_a_options) begin
      (txt, nil(), _) => begin
        txt
      end

      (txt, i_class__part <| rest, a_options) => begin
        @assign x_idx = Tpl.getIteri_i0(txt)
        @assign txt = dumpClassPart(txt, i_class__part, x_idx, a_options)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_22(txt, rest, a_options)
        txt
      end
    end
  end
  return out_txt
end

function lm_23(in_txt::Tpl.Text, in_items::List{<:Absyn.ElementArg})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Absyn.ElementArg}
    local i_mod::Absyn.ElementArg
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_mod <| rest) => begin
        @assign txt = dumpElementArg(txt, i_mod)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_23(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function fun_24(in_txt::Tpl.Text, in_a_modifications::List{<:Absyn.ElementArg})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_modifications::List{Absyn.ElementArg}
    @match (in_txt, in_a_modifications) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_modifications) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("("))
        @assign txt = Tpl.pushIter(
          txt,
          Tpl.ITER_OPTIONS(
            0,
            NONE(),
            SOME(Tpl.ST_STRING(", ")),
            0,
            0,
            Tpl.ST_NEW_LINE(),
            0,
            Tpl.ST_NEW_LINE(),
          ),
        )
        @assign txt = lm_23(txt, i_modifications)
        @assign txt = Tpl.popIter(txt)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(")"))
        txt
      end
    end
  end
  return out_txt
end

function lm_25(in_txt::Tpl.Text, in_items::List{<:Absyn.Annotation})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Absyn.Annotation}
    local i_a::Absyn.Annotation
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_a <| rest) => begin
        @assign txt = dumpAnnotation(txt, i_a)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_25(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function fun_26(in_txt::Tpl.Text, in_a_ann__str::Tpl.Text)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_ann__str::Tpl.Text
    @match (in_txt, in_a_ann__str) begin
      (txt, Tpl.MEM_TEXT(tokens = nil())) => begin
        txt
      end

      (txt, i_ann__str) => begin
        @assign txt = Tpl.writeText(txt, i_ann__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(";"))
        txt
      end
    end
  end
  return out_txt
end

function lm_27(in_txt::Tpl.Text, in_items::List{<:Absyn.Path})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Absyn.Path}
    local i_fn::Absyn.Path
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_fn <| rest) => begin
        @assign txt = dumpPath(txt, i_fn)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_27(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function lm_28(in_txt::Tpl.Text, in_items::List{<:Absyn.Ident})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Absyn.Ident}
    local i_var::Absyn.Ident
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_var <| rest) => begin
        @assign txt = Tpl.writeStr(txt, i_var)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_28(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function dumpClassDef(
  in_txt::Tpl.Text,
  in_a_cdef::Absyn.ClassDef,
  in_a_cls__name::String,
  in_a_options::Dump.DumpOptions,
)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local a_cls__name::String
    local a_options::Dump.DumpOptions
    local i_vars::List{Absyn.Ident}
    local i_functionName::Absyn.Path
    local i_functionNames::List{Absyn.Path}
    local i_enumLiterals::Absyn.EnumDef
    local i_baseClassName::Absyn.Ident
    local i_modifications::List{Absyn.ElementArg}
    local i_parts::List{Absyn.ClassPart}
    local i_comment_1::Option{Absyn.Comment}
    local i_arguments::List{Absyn.ElementArg}
    local i_typeSpec::Absyn.TypeSpec
    local i_attributes::Absyn.ElementAttributes
    local i_classParts::List{Absyn.ClassPart}
    local i_comment::Option{String}
    local i_ann::List{Absyn.Annotation}
    local i_typeVars::List{String}
    local l_vars__str::Tpl.Text
    local l_fn__str::Tpl.Text
    local l_funcs__str::Tpl.Text
    local l_enum__str::Tpl.Text
    local ret_8::List{Absyn.Annotation}
    local l_mod__str::Tpl.Text
    local l_ty__str::Tpl.Text
    local l_attr__str::Tpl.Text
    local l_body__str::Tpl.Text
    local l_cmt__str::Tpl.Text
    local ret_2::List{Absyn.Annotation}
    local l_ann__str::Tpl.Text
    local l_tvs__str::Tpl.Text
    @match (in_txt, in_a_cdef, in_a_cls__name, in_a_options) begin
      (
        txt,
        Absyn.PARTS(
          typeVars = i_typeVars,
          ann = i_ann,
          comment = i_comment,
          classParts = i_classParts,
        ),
        a_cls__name,
        a_options,
      ) => begin
        @assign l_tvs__str = fun_16(Tpl.emptyTxt, i_typeVars)
        @assign ret_2 = listReverse(i_ann)
        @assign l_ann__str = Tpl.pushIter(
          Tpl.emptyTxt,
          Tpl.ITER_OPTIONS(
            0,
            NONE(),
            SOME(Tpl.ST_LINE(";\\n")),
            0,
            0,
            Tpl.ST_NEW_LINE(),
            0,
            Tpl.ST_NEW_LINE(),
          ),
        )
        @assign l_ann__str = lm_17(l_ann__str, ret_2)
        @assign l_ann__str = Tpl.popIter(l_ann__str)
        @assign l_cmt__str = dumpStringCommentOption(Tpl.emptyTxt, i_comment)
        @assign l_body__str = Tpl.pushIter(
          Tpl.emptyTxt,
          Tpl.ITER_OPTIONS(
            0,
            NONE(),
            SOME(Tpl.ST_STRING("")),
            0,
            0,
            Tpl.ST_NEW_LINE(),
            0,
            Tpl.ST_NEW_LINE(),
          ),
        )
        @assign l_body__str = lm_18(l_body__str, i_classParts, a_options)
        @assign l_body__str = Tpl.popIter(l_body__str)
        @assign txt = Tpl.writeStr(txt, a_cls__name)
        @assign txt = Tpl.writeText(txt, l_tvs__str)
        @assign txt = Tpl.writeText(txt, l_cmt__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_NEW_LINE())
        @assign txt = Tpl.writeText(txt, l_body__str)
        @assign txt = Tpl.softNewLine(txt)
        @assign txt = Tpl.pushBlock(txt, Tpl.BT_INDENT(2))
        @assign txt = fun_19(txt, l_ann__str)
        @assign txt = Tpl.softNewLine(txt)
        @assign txt = Tpl.popBlock(txt)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("end "))
        @assign txt = Tpl.writeStr(txt, a_cls__name)
        txt
      end

      (
        txt,
        Absyn.DERIVED(
          attributes = i_attributes,
          typeSpec = i_typeSpec,
          arguments = i_arguments,
          comment = i_comment_1,
        ),
        a_cls__name,
        _,
      ) => begin
        @assign l_attr__str = dumpElementAttr(Tpl.emptyTxt, i_attributes)
        @assign l_ty__str = dumpTypeSpec(Tpl.emptyTxt, i_typeSpec)
        @assign l_mod__str = fun_21(Tpl.emptyTxt, i_arguments)
        @assign l_cmt__str = dumpCommentOpt(Tpl.emptyTxt, i_comment_1)
        @assign txt = Tpl.writeStr(txt, a_cls__name)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(" = "))
        @assign txt = Tpl.writeText(txt, l_attr__str)
        @assign txt = Tpl.writeText(txt, l_ty__str)
        @assign txt = Tpl.writeText(txt, l_mod__str)
        @assign txt = Tpl.writeText(txt, l_cmt__str)
        txt
      end

      (
        txt,
        Absyn.CLASS_EXTENDS(
          parts = i_parts,
          modifications = i_modifications,
          comment = i_comment,
          ann = i_ann,
          baseClassName = i_baseClassName,
        ),
        a_cls__name,
        a_options,
      ) => begin
        @assign l_body__str = Tpl.pushIter(
          Tpl.emptyTxt,
          Tpl.ITER_OPTIONS(
            0,
            NONE(),
            SOME(Tpl.ST_NEW_LINE()),
            0,
            0,
            Tpl.ST_NEW_LINE(),
            0,
            Tpl.ST_NEW_LINE(),
          ),
        )
        @assign l_body__str = lm_22(l_body__str, i_parts, a_options)
        @assign l_body__str = Tpl.popIter(l_body__str)
        @assign l_mod__str = fun_24(Tpl.emptyTxt, i_modifications)
        @assign l_cmt__str = dumpStringCommentOption(Tpl.emptyTxt, i_comment)
        @assign ret_8 = listReverse(i_ann)
        @assign l_ann__str = Tpl.pushIter(
          Tpl.emptyTxt,
          Tpl.ITER_OPTIONS(
            0,
            NONE(),
            SOME(Tpl.ST_LINE(";\\n")),
            0,
            0,
            Tpl.ST_NEW_LINE(),
            0,
            Tpl.ST_NEW_LINE(),
          ),
        )
        @assign l_ann__str = lm_25(l_ann__str, ret_8)
        @assign l_ann__str = Tpl.popIter(l_ann__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("extends "))
        @assign txt = Tpl.writeStr(txt, i_baseClassName)
        @assign txt = Tpl.writeText(txt, l_mod__str)
        @assign txt = Tpl.writeText(txt, l_cmt__str)
        @assign txt = Tpl.softNewLine(txt)
        @assign txt = Tpl.pushBlock(txt, Tpl.BT_INDENT(2))
        @assign txt = Tpl.writeText(txt, l_body__str)
        @assign txt = Tpl.softNewLine(txt)
        @assign txt = fun_26(txt, l_ann__str)
        @assign txt = Tpl.softNewLine(txt)
        @assign txt = Tpl.popBlock(txt)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("end "))
        @assign txt = Tpl.writeStr(txt, a_cls__name)
        txt
      end

      (
        txt,
        Absyn.ENUMERATION(enumLiterals = i_enumLiterals, comment = i_comment_1),
        a_cls__name,
        _,
      ) => begin
        @assign l_enum__str = dumpEnumDef(Tpl.emptyTxt, i_enumLiterals)
        @assign l_cmt__str = dumpCommentOpt(Tpl.emptyTxt, i_comment_1)
        @assign txt = Tpl.writeStr(txt, a_cls__name)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(" = enumeration("))
        @assign txt = Tpl.writeText(txt, l_enum__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(")"))
        @assign txt = Tpl.writeText(txt, l_cmt__str)
        txt
      end

      (
        txt,
        Absyn.OVERLOAD(functionNames = i_functionNames, comment = i_comment_1),
        a_cls__name,
        _,
      ) => begin
        @assign l_funcs__str = Tpl.pushIter(
          Tpl.emptyTxt,
          Tpl.ITER_OPTIONS(
            0,
            NONE(),
            SOME(Tpl.ST_STRING(", ")),
            0,
            0,
            Tpl.ST_NEW_LINE(),
            0,
            Tpl.ST_NEW_LINE(),
          ),
        )
        @assign l_funcs__str = lm_27(l_funcs__str, i_functionNames)
        @assign l_funcs__str = Tpl.popIter(l_funcs__str)
        @assign l_cmt__str = dumpCommentOpt(Tpl.emptyTxt, i_comment_1)
        @assign txt = Tpl.writeStr(txt, a_cls__name)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(" = overload("))
        @assign txt = Tpl.writeText(txt, l_funcs__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(")"))
        @assign txt = Tpl.writeText(txt, l_cmt__str)
        txt
      end

      (txt, Absyn.PDER(functionName = i_functionName, vars = i_vars), a_cls__name, _) =>
        begin
          @assign l_fn__str = dumpPath(Tpl.emptyTxt, i_functionName)
          @assign l_vars__str = Tpl.pushIter(
            Tpl.emptyTxt,
            Tpl.ITER_OPTIONS(
              0,
              NONE(),
              SOME(Tpl.ST_STRING(", ")),
              0,
              0,
              Tpl.ST_NEW_LINE(),
              0,
              Tpl.ST_NEW_LINE(),
            ),
          )
          @assign l_vars__str = lm_28(l_vars__str, i_vars)
          @assign l_vars__str = Tpl.popIter(l_vars__str)
          @assign txt = Tpl.writeStr(txt, a_cls__name)
          @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(" = der("))
          @assign txt = Tpl.writeText(txt, l_fn__str)
          @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(", "))
          @assign txt = Tpl.writeText(txt, l_vars__str)
          @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(")"))
          txt
        end

      (txt, _, _, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function lm_30(in_txt::Tpl.Text, in_items::List{<:Absyn.EnumLiteral})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Absyn.EnumLiteral}
    local i_lit::Absyn.EnumLiteral
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_lit <| rest) => begin
        @assign txt = dumpEnumLiteral(txt, i_lit)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_30(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function dumpEnumDef(in_txt::Tpl.Text, in_a_enum__def::Absyn.EnumDef)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_enumLiterals::List{Absyn.EnumLiteral}
    @match (in_txt, in_a_enum__def) begin
      (txt, Absyn.ENUMLITERALS(enumLiterals = i_enumLiterals)) => begin
        @assign txt = Tpl.pushIter(
          txt,
          Tpl.ITER_OPTIONS(
            0,
            NONE(),
            SOME(Tpl.ST_STRING(", ")),
            0,
            0,
            Tpl.ST_NEW_LINE(),
            0,
            Tpl.ST_NEW_LINE(),
          ),
        )
        @assign txt = lm_30(txt, i_enumLiterals)
        @assign txt = Tpl.popIter(txt)
        txt
      end

      (txt, Absyn.ENUM_COLON(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(":"))
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function dumpEnumLiteral(in_txt::Tpl.Text, in_a_lit::Absyn.EnumLiteral)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_literal::Absyn.Ident
    local i_comment::Option{Absyn.Comment}
    local l_cmt__str::Tpl.Text
    @match (in_txt, in_a_lit) begin
      (txt, Absyn.ENUMLITERAL(comment = i_comment, literal = i_literal)) => begin
        @assign l_cmt__str = dumpCommentOpt(Tpl.emptyTxt, i_comment)
        @assign txt = Tpl.writeStr(txt, i_literal)
        @assign txt = Tpl.writeText(txt, l_cmt__str)
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function fun_33(in_txt::Tpl.Text, in_a_encapsulatedPrefix::Bool)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    @match (in_txt, in_a_encapsulatedPrefix) begin
      (txt, false) => begin
        txt
      end

      (txt, _) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("encapsulated "))
        txt
      end
    end
  end
  return out_txt
end

function fun_34(in_txt::Tpl.Text, in_a_partialPrefix::Bool)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    @match (in_txt, in_a_partialPrefix) begin
      (txt, false) => begin
        txt
      end

      (txt, _) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("partial "))
        txt
      end
    end
  end
  return out_txt
end

function fun_35(
  in_txt::Tpl.Text,
  in_a_cls::Absyn.Class,
  in_a_redecl__str::String,
  in_a_repl__str::String,
  in_a_io__str::String,
)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local a_redecl__str::String
    local a_repl__str::String
    local a_io__str::String
    local i_finalPrefix::Bool
    local i_partialPrefix::Bool
    local i_encapsulatedPrefix::Bool
    local l_fin__str::Tpl.Text
    local l_partial__str::Tpl.Text
    local l_enc__str::Tpl.Text
    @match (in_txt, in_a_cls, in_a_redecl__str, in_a_repl__str, in_a_io__str) begin
      (
        txt,
        Absyn.CLASS(
          encapsulatedPrefix = i_encapsulatedPrefix,
          partialPrefix = i_partialPrefix,
          finalPrefix = i_finalPrefix,
        ),
        a_redecl__str,
        a_repl__str,
        a_io__str,
      ) => begin
        @assign l_enc__str = fun_33(Tpl.emptyTxt, i_encapsulatedPrefix)
        @assign l_partial__str = fun_34(Tpl.emptyTxt, i_partialPrefix)
        @assign l_fin__str = dumpFinal(Tpl.emptyTxt, i_finalPrefix)
        @assign txt = Tpl.writeStr(txt, a_redecl__str)
        @assign txt = Tpl.writeText(txt, l_fin__str)
        @assign txt = Tpl.writeStr(txt, a_io__str)
        @assign txt = Tpl.writeStr(txt, a_repl__str)
        @assign txt = Tpl.writeText(txt, l_enc__str)
        @assign txt = Tpl.writeText(txt, l_partial__str)
        txt
      end

      (txt, _, _, _, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function dumpClassPrefixes(
  txt::Tpl.Text,
  a_cls::Absyn.Class,
  a_final__str::String,
  a_redecl__str::String,
  a_repl__str::String,
  a_io__str::String,
)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = fun_35(txt, a_cls, a_redecl__str, a_repl__str, a_io__str)
  return out_txt
end

function fun_37(
  in_txt::Tpl.Text,
  in_a_functionRestriction::Absyn.FunctionRestriction,
)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    @match (in_txt, in_a_functionRestriction) begin
      (txt, Absyn.FR_NORMAL_FUNCTION(purity = Absyn.IMPURE(__))) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("impure "))
        txt
      end

      (txt, Absyn.FR_NORMAL_FUNCTION(purity = Absyn.PURE(__))) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("pure "))
        txt
      end

      (txt, Absyn.FR_NORMAL_FUNCTION(purity = Absyn.NO_PURITY(__))) => begin
        txt
      end

      (txt, Absyn.FR_OPERATOR_FUNCTION(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("operator "))
        txt
      end

      (txt, Absyn.FR_PARALLEL_FUNCTION(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("parallel "))
        txt
      end

      (txt, Absyn.FR_KERNEL_FUNCTION(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("kernel "))
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function lm_38(in_txt::Tpl.Text, in_items::List{<:String})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{String}
    local i_tv::String
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_tv <| rest) => begin
        @assign txt = Tpl.writeStr(txt, i_tv)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_38(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function fun_39(in_txt::Tpl.Text, in_a_typeVars::List{<:String})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_typeVars::List{String}
    @match (in_txt, in_a_typeVars) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_typeVars) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("<"))
        @assign txt = Tpl.pushIter(
          txt,
          Tpl.ITER_OPTIONS(
            0,
            NONE(),
            SOME(Tpl.ST_STRING(",")),
            0,
            0,
            Tpl.ST_NEW_LINE(),
            0,
            Tpl.ST_NEW_LINE(),
          ),
        )
        @assign txt = lm_38(txt, i_typeVars)
        @assign txt = Tpl.popIter(txt)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(">"))
        txt
      end
    end
  end
  return out_txt
end

function dumpRestriction(in_txt::Tpl.Text, in_a_restriction::Absyn.Restriction)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_typeVars::List{String}
    local i_functionRestriction::Absyn.FunctionRestriction
    local l_prefix__str::Tpl.Text
    @match (in_txt, in_a_restriction) begin
      (txt, Absyn.R_CLASS(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("class"))
        txt
      end

      (txt, Absyn.R_OPTIMIZATION(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("optimization"))
        txt
      end

      (txt, Absyn.R_MODEL(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("model"))
        txt
      end

      (txt, Absyn.R_RECORD(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("record"))
        txt
      end

      (txt, Absyn.R_BLOCK(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("block"))
        txt
      end

      (txt, Absyn.R_CONNECTOR(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("connector"))
        txt
      end

      (txt, Absyn.R_EXP_CONNECTOR(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("expandable connector"))
        txt
      end

      (txt, Absyn.R_TYPE(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("type"))
        txt
      end

      (txt, Absyn.R_PACKAGE(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("package"))
        txt
      end

      (txt, Absyn.R_FUNCTION(functionRestriction = i_functionRestriction)) => begin
        @assign l_prefix__str = fun_37(Tpl.emptyTxt, i_functionRestriction)
        @assign txt = Tpl.writeText(txt, l_prefix__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("function"))
        txt
      end

      (txt, Absyn.R_OPERATOR(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("operator"))
        txt
      end

      (txt, Absyn.R_OPERATOR_RECORD(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("operator record"))
        txt
      end

      (txt, Absyn.R_ENUMERATION(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("enumeration"))
        txt
      end

      (txt, Absyn.R_PREDEFINED_INTEGER(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("Integer"))
        txt
      end

      (txt, Absyn.R_PREDEFINED_REAL(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("Real"))
        txt
      end

      (txt, Absyn.R_PREDEFINED_STRING(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("String"))
        txt
      end

      (txt, Absyn.R_PREDEFINED_BOOLEAN(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("Boolean"))
        txt
      end

      (txt, Absyn.R_PREDEFINED_ENUMERATION(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("enumeration(:)"))
        txt
      end

      (txt, Absyn.R_UNIONTYPE(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("uniontype"))
        txt
      end

      (txt, Absyn.R_METARECORD(typeVars = i_typeVars)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("metarecord"))
        @assign txt = fun_39(txt, i_typeVars)
        txt
      end

      (txt, Absyn.R_UNKNOWN(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("*unknown*"))
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function fun_41(in_txt::Tpl.Text, in_a_idx::Integer)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    @match (in_txt, in_a_idx) begin
      (txt, 0) => begin
        txt
      end

      (txt, _) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("public"))
        txt
      end
    end
  end
  return out_txt
end

function lm_42(in_txt::Tpl.Text, in_items::List{<:Absyn.Exp})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Absyn.Exp}
    local i_exp::Absyn.Exp
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_exp <| rest) => begin
        @assign txt = dumpExp(txt, i_exp)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_42(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function lm_43(in_txt::Tpl.Text, in_items::List{<:Absyn.EquationItem})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Absyn.EquationItem}
    local i_eq::Absyn.EquationItem
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_eq <| rest) => begin
        @assign txt = dumpEquationItem(txt, i_eq)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_43(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function lm_44(in_txt::Tpl.Text, in_items::List{<:Absyn.EquationItem})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Absyn.EquationItem}
    local i_eq::Absyn.EquationItem
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_eq <| rest) => begin
        @assign txt = dumpEquationItem(txt, i_eq)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_44(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function lm_45(in_txt::Tpl.Text, in_items::List{<:Absyn.AlgorithmItem})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Absyn.AlgorithmItem}
    local i_eq::Absyn.AlgorithmItem
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_eq <| rest) => begin
        @assign txt = dumpAlgorithmItem(txt, i_eq)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_45(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function lm_46(in_txt::Tpl.Text, in_items::List{<:Absyn.AlgorithmItem})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Absyn.AlgorithmItem}
    local i_eq::Absyn.AlgorithmItem
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_eq <| rest) => begin
        @assign txt = dumpAlgorithmItem(txt, i_eq)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_46(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function fun_47(in_txt::Tpl.Text, in_a_annotation__::Option{<:Absyn.Annotation})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_ann::Absyn.Annotation
    @match (in_txt, in_a_annotation__) begin
      (txt, SOME(i_ann)) => begin
        @assign txt = Tpl.pushBlock(txt, Tpl.BT_INDENT(1))
        @assign txt = dumpAnnotation(txt, i_ann)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(";"))
        @assign txt = Tpl.popBlock(txt)
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function fun_48(in_txt::Tpl.Text, in_a_funcName::Option{<:Absyn.Ident})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_fn::Absyn.Ident
    @match (in_txt, in_a_funcName) begin
      (txt, SOME(i_fn)) => begin
        @assign txt = Tpl.writeStr(txt, i_fn)
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function fun_49(in_txt::Tpl.Text, in_a_lang::Option{<:String})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_l::String
    @match (in_txt, in_a_lang) begin
      (txt, SOME(i_l)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("\\"))
        @assign txt = Tpl.writeStr(txt, i_l)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("\\ "))
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function fun_50(in_txt::Tpl.Text, in_a_output__::Option{<:Absyn.ComponentRef})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_o::Absyn.ComponentRef
    @match (in_txt, in_a_output__) begin
      (txt, SOME(i_o)) => begin
        @assign txt = dumpCref(txt, i_o)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(" = "))
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function fun_51(in_txt::Tpl.Text, in_a_fn__str::Tpl.Text)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    @match (in_txt, in_a_fn__str) begin
      (txt, Tpl.MEM_TEXT(tokens = nil())) => begin
        txt
      end

      (txt, _) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("()"))
        txt
      end
    end
  end
  return out_txt
end

function lm_52(in_txt::Tpl.Text, in_items::List{<:Absyn.Exp})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Absyn.Exp}
    local i_arg::Absyn.Exp
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_arg <| rest) => begin
        @assign txt = dumpExp(txt, i_arg)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_52(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function fun_53(
  in_txt::Tpl.Text,
  in_a_args::List{<:Absyn.Exp},
  in_a_fn__str::Tpl.Text,
)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local a_fn__str::Tpl.Text
    local i_args::List{Absyn.Exp}
    @match (in_txt, in_a_args, in_a_fn__str) begin
      (txt, nil(), a_fn__str) => begin
        @assign txt = fun_51(txt, a_fn__str)
        txt
      end

      (txt, i_args, _) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("("))
        @assign txt = Tpl.pushIter(
          txt,
          Tpl.ITER_OPTIONS(
            0,
            NONE(),
            SOME(Tpl.ST_STRING(", ")),
            0,
            0,
            Tpl.ST_NEW_LINE(),
            0,
            Tpl.ST_NEW_LINE(),
          ),
        )
        @assign txt = lm_52(txt, i_args)
        @assign txt = Tpl.popIter(txt)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(")"))
        txt
      end
    end
  end
  return out_txt
end

function fun_54(
  in_txt::Tpl.Text,
  in_a_externalDecl::Absyn.ExternalDecl,
  in_a_ann__str::Tpl.Text,
)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local a_ann__str::Tpl.Text
    local i_annotation__::Option{Absyn.Annotation}
    local i_args::List{Absyn.Exp}
    local i_output__::Option{Absyn.ComponentRef}
    local i_lang::Option{String}
    local i_funcName::Option{Absyn.Ident}
    local l_ann2__str::Tpl.Text
    local l_args__str::Tpl.Text
    local l_output__str::Tpl.Text
    local l_lang__str::Tpl.Text
    local l_fn__str::Tpl.Text
    @match (in_txt, in_a_externalDecl, in_a_ann__str) begin
      (
        txt,
        Absyn.EXTERNALDECL(
          funcName = i_funcName,
          lang = i_lang,
          output_ = i_output__,
          args = i_args,
          annotation_ = i_annotation__,
        ),
        a_ann__str,
      ) => begin
        @assign l_fn__str = fun_48(Tpl.emptyTxt, i_funcName)
        @assign l_lang__str = fun_49(Tpl.emptyTxt, i_lang)
        @assign l_output__str = fun_50(Tpl.emptyTxt, i_output__)
        @assign l_args__str = fun_53(Tpl.emptyTxt, i_args, l_fn__str)
        @assign l_ann2__str = dumpAnnotationOptSpace(Tpl.emptyTxt, i_annotation__)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_NEW_LINE())
        @assign txt = Tpl.pushBlock(txt, Tpl.BT_INDENT(2))
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("external "))
        @assign txt = Tpl.writeText(txt, l_lang__str)
        @assign txt = Tpl.writeText(txt, l_output__str)
        @assign txt = Tpl.writeText(txt, l_fn__str)
        @assign txt = Tpl.writeText(txt, l_args__str)
        @assign txt = Tpl.writeText(txt, l_ann2__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(";"))
        @assign txt = Tpl.writeText(txt, a_ann__str)
        @assign txt = Tpl.popBlock(txt)
        txt
      end

      (txt, _, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function dumpClassPart(
  in_txt::Tpl.Text,
  in_a_class__part::Absyn.ClassPart,
  in_a_idx::Integer,
  in_a_options::Dump.DumpOptions,
)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local a_idx::Integer
    local a_options::Dump.DumpOptions
    local i_externalDecl::Absyn.ExternalDecl
    local i_annotation__::Option{Absyn.Annotation}
    local i_contents_3::List{Absyn.AlgorithmItem}
    local i_contents_2::List{Absyn.EquationItem}
    local i_contents_1::List{Absyn.Exp}
    local i_contents::List{Absyn.ElementItem}
    local l_ann__str::Tpl.Text
    local l_el__str::Tpl.Text
    local l_section__str::Tpl.Text
    @match (in_txt, in_a_class__part, in_a_idx, in_a_options) begin
      (txt, Absyn.PUBLIC(contents = i_contents), a_idx, a_options) => begin
        @assign l_section__str = fun_41(Tpl.emptyTxt, a_idx)
        @assign l_el__str =
          dumpElementItems(Tpl.emptyTxt, i_contents, "", true, a_options)
        @assign txt = Tpl.writeText(txt, l_section__str)
        @assign txt = Tpl.softNewLine(txt)
        @assign txt = Tpl.pushBlock(txt, Tpl.BT_INDENT(2))
        @assign txt = Tpl.writeText(txt, l_el__str)
        @assign txt = Tpl.popBlock(txt)
        txt
      end

      (txt, Absyn.PROTECTED(contents = i_contents), _, a_options) => begin
        @assign l_el__str = dumpElementItems(Tpl.emptyTxt, i_contents, "", true, a_options)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_LINE("protected\\n"))
        @assign txt = Tpl.pushBlock(txt, Tpl.BT_INDENT(2))
        @assign txt = Tpl.writeText(txt, l_el__str)
        @assign txt = Tpl.popBlock(txt)
        txt
      end

      (txt, Absyn.CONSTRAINTS(contents = i_contents_1), _, _) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_LINE("constraint\\n"))
        @assign txt = Tpl.pushBlock(txt, Tpl.BT_INDENT(2))
        @assign txt = Tpl.pushIter(
          txt,
          Tpl.ITER_OPTIONS(
            0,
            NONE(),
            SOME(Tpl.ST_STRING("; ")),
            0,
            0,
            Tpl.ST_NEW_LINE(),
            0,
            Tpl.ST_NEW_LINE(),
          ),
        )
        @assign txt = lm_42(txt, i_contents_1)
        @assign txt = Tpl.popIter(txt)
        @assign txt = Tpl.popBlock(txt)
        txt
      end

      (txt, Absyn.EQUATIONS(contents = i_contents_2), _, _) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_LINE("equation\\n"))
        @assign txt = Tpl.pushBlock(txt, Tpl.BT_INDENT(2))
        @assign txt = Tpl.pushIter(
          txt,
          Tpl.ITER_OPTIONS(
            0,
            NONE(),
            SOME(Tpl.ST_NEW_LINE()),
            0,
            0,
            Tpl.ST_NEW_LINE(),
            0,
            Tpl.ST_NEW_LINE(),
          ),
        )
        @assign txt = lm_43(txt, i_contents_2)
        @assign txt = Tpl.popIter(txt)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_NEW_LINE())
        @assign txt = Tpl.popBlock(txt)
        txt
      end

      (txt, Absyn.INITIALEQUATIONS(contents = i_contents_2), _, _) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_LINE("initial equation\\n"))
        @assign txt = Tpl.pushBlock(txt, Tpl.BT_INDENT(2))
        @assign txt = Tpl.pushIter(
          txt,
          Tpl.ITER_OPTIONS(
            0,
            NONE(),
            SOME(Tpl.ST_NEW_LINE()),
            0,
            0,
            Tpl.ST_NEW_LINE(),
            0,
            Tpl.ST_NEW_LINE(),
          ),
        )
        @assign txt = lm_44(txt, i_contents_2)
        @assign txt = Tpl.popIter(txt)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_NEW_LINE())
        @assign txt = Tpl.popBlock(txt)
        txt
      end

      (txt, Absyn.ALGORITHMS(contents = i_contents_3), _, _) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_LINE("algorithm\\n"))
        @assign txt = Tpl.pushBlock(txt, Tpl.BT_INDENT(2))
        @assign txt = Tpl.pushIter(
          txt,
          Tpl.ITER_OPTIONS(
            0,
            NONE(),
            SOME(Tpl.ST_NEW_LINE()),
            0,
            0,
            Tpl.ST_NEW_LINE(),
            0,
            Tpl.ST_NEW_LINE(),
          ),
        )
        @assign txt = lm_45(txt, i_contents_3)
        @assign txt = Tpl.popIter(txt)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_NEW_LINE())
        @assign txt = Tpl.popBlock(txt)
        txt
      end

      (txt, Absyn.INITIALALGORITHMS(contents = i_contents_3), _, _) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_LINE("initial algorithm\\n"))
        @assign txt = Tpl.pushBlock(txt, Tpl.BT_INDENT(2))
        @assign txt = Tpl.pushIter(
          txt,
          Tpl.ITER_OPTIONS(
            0,
            NONE(),
            SOME(Tpl.ST_NEW_LINE()),
            0,
            0,
            Tpl.ST_NEW_LINE(),
            0,
            Tpl.ST_NEW_LINE(),
          ),
        )
        @assign txt = lm_46(txt, i_contents_3)
        @assign txt = Tpl.popIter(txt)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_NEW_LINE())
        @assign txt = Tpl.popBlock(txt)
        txt
      end

      (
        txt,
        Absyn.EXTERNAL(annotation_ = i_annotation__, externalDecl = i_externalDecl),
        _,
        _,
      ) => begin
        @assign l_ann__str = fun_47(Tpl.emptyTxt, i_annotation__)
        @assign txt = fun_54(txt, i_externalDecl, l_ann__str)
        txt
      end

      (txt, _, _, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function fun_56(
  in_txt::Tpl.Text,
  in_a_first::Bool,
  in_a_prevSpacing::String,
  in_a_spacing::Tpl.Text,
)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local a_prevSpacing::String
    local a_spacing::Tpl.Text
    @match (in_txt, in_a_first, in_a_prevSpacing, in_a_spacing) begin
      (txt, false, a_prevSpacing, a_spacing) => begin
        @assign txt =
          dumpElementItemPreSpacing(txt, Tpl.textString(a_spacing), a_prevSpacing)
        txt
      end

      (txt, _, _, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function fun_57(
  in_txt::Tpl.Text,
  in_a_rest__str::Tpl.Text,
  in_a_spacing::Tpl.Text,
)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local a_spacing::Tpl.Text
    @match (in_txt, in_a_rest__str, in_a_spacing) begin
      (txt, Tpl.MEM_TEXT(tokens = nil()), _) => begin
        txt
      end

      (txt, _, a_spacing) => begin
        @assign txt = Tpl.writeText(txt, a_spacing)
        txt
      end
    end
  end
  return out_txt
end

function fun_58(in_txt::Tpl.Text, in_a_rest__str::Tpl.Text)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_rest__str::Tpl.Text
    @match (in_txt, in_a_rest__str) begin
      (txt, Tpl.MEM_TEXT(tokens = nil())) => begin
        txt
      end

      (txt, i_rest__str) => begin
        @assign txt = Tpl.writeText(txt, i_rest__str)
        txt
      end
    end
  end
  return out_txt
end

function dumpElementItems(
  in_txt::Tpl.Text,
  in_a_items::List{<:Absyn.ElementItem},
  in_a_prevSpacing::String,
  in_a_first::Bool,
  in_a_options::Dump.DumpOptions,
)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local a_prevSpacing::String
    local a_first::Bool
    local a_options::Dump.DumpOptions
    local i_rest__items::List{Absyn.ElementItem}
    local i_item::Absyn.ElementItem
    local l_post__spacing::Tpl.Text
    local l_rest__str::Tpl.Text
    local l_item__str::Tpl.Text
    local l_pre__spacing::Tpl.Text
    local l_spacing::Tpl.Text
    @match (in_txt, in_a_items, in_a_prevSpacing, in_a_first, in_a_options) begin
      (txt, i_item <| i_rest__items, a_prevSpacing, a_first, a_options) => begin
        @assign l_spacing = dumpElementItemSpacing(Tpl.emptyTxt, i_item)
        @assign l_pre__spacing = fun_56(Tpl.emptyTxt, a_first, a_prevSpacing, l_spacing)
        @assign l_item__str = dumpElementItem(Tpl.emptyTxt, i_item, a_options)
        @assign l_rest__str = dumpElementItems(
          Tpl.emptyTxt,
          i_rest__items,
          Tpl.textString(l_spacing),
          false,
          a_options,
        )
        @assign l_post__spacing = fun_57(Tpl.emptyTxt, l_rest__str, l_spacing)
        @assign txt = Tpl.writeText(txt, l_pre__spacing)
        @assign txt = Tpl.softNewLine(txt)
        @assign txt = Tpl.writeText(txt, l_item__str)
        @assign txt = Tpl.writeText(txt, l_post__spacing)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_NEW_LINE())
        @assign txt = fun_58(txt, l_rest__str)
        txt
      end

      (txt, _, _, _, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function fun_60(
  in_txt::Tpl.Text,
  in_a_prevSpacing::String,
  in_a_curSpacing::String,
)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local a_curSpacing::String
    @match (in_txt, in_a_prevSpacing, in_a_curSpacing) begin
      (txt, "", a_curSpacing) => begin
        @assign txt = Tpl.writeStr(txt, a_curSpacing)
        txt
      end

      (txt, _, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function dumpElementItemPreSpacing(
  txt::Tpl.Text,
  a_curSpacing::String,
  a_prevSpacing::String,
)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = fun_60(txt, a_prevSpacing, a_curSpacing)
  return out_txt
end

function dumpElementItemSpacing(in_txt::Tpl.Text, in_a_item::Absyn.ElementItem)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_cdef::Absyn.ClassDef
    @match (in_txt, in_a_item) begin
      (
        txt,
        Absyn.ELEMENTITEM(
          element = Absyn.ELEMENT(
            specification = Absyn.CLASSDEF(class_ = Absyn.CLASS(body = i_cdef)),
          ),
        ),
      ) => begin
        @assign txt = dumpClassDefSpacing(txt, i_cdef)
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function dumpClassDefSpacing(in_txt::Tpl.Text, in_a_cdef::Absyn.ClassDef)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    @match (in_txt, in_a_cdef) begin
      (txt, Absyn.PARTS(typeVars = _)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_NEW_LINE())
        txt
      end

      (txt, Absyn.CLASS_EXTENDS(baseClassName = _)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_NEW_LINE())
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function dumpElementItem(
  in_txt::Tpl.Text,
  in_a_eitem::Absyn.ElementItem,
  in_a_options::Dump.DumpOptions,
)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local a_options::Dump.DumpOptions
    local i_comment::String
    local i_element::Absyn.Element
    local ret_0::String
    @match (in_txt, in_a_eitem, in_a_options) begin
      (txt, Absyn.ELEMENTITEM(element = i_element), a_options) => begin
        @assign txt = dumpElement(txt, i_element, a_options)
        txt
      end

      (txt, Absyn.LEXER_COMMENT(comment = i_comment), _) => begin
        @assign ret_0 = System.trimWhitespace(i_comment)
        @assign txt = Tpl.writeStr(txt, ret_0)
        txt
      end

      (txt, _, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function fun_65(
  in_txt::Tpl.Text,
  in_a_redeclareKeywords::Option{<:Absyn.RedeclareKeywords},
)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_re::Absyn.RedeclareKeywords
    @match (in_txt, in_a_redeclareKeywords) begin
      (txt, SOME(i_re)) => begin
        @assign txt = dumpRedeclare(txt, i_re)
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function fun_66(
  in_txt::Tpl.Text,
  in_a_redeclareKeywords::Option{<:Absyn.RedeclareKeywords},
)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_re::Absyn.RedeclareKeywords
    @match (in_txt, in_a_redeclareKeywords) begin
      (txt, SOME(i_re)) => begin
        @assign txt = dumpReplaceable(txt, i_re)
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function fun_67(
  in_txt::Tpl.Text,
  in_a_constrainClass::Option{<:Absyn.ConstrainClass},
)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_cc::Absyn.ConstrainClass
    @match (in_txt, in_a_constrainClass) begin
      (txt, SOME(i_cc)) => begin
        @assign txt = dumpConstrainClass(txt, i_cc)
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function fun_68(
  in_txt::Tpl.Text,
  in_mArg::Bool,
  in_a_constrainClass::Option{<:Absyn.ConstrainClass},
  in_a_options::Dump.DumpOptions,
  in_a_specification::Absyn.ElementSpec,
  in_a_innerOuter::Absyn.InnerOuter,
  in_a_redeclareKeywords::Option{<:Absyn.RedeclareKeywords},
  in_a_finalPrefix::Bool,
)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local a_constrainClass::Option{Absyn.ConstrainClass}
    local a_options::Dump.DumpOptions
    local a_specification::Absyn.ElementSpec
    local a_innerOuter::Absyn.InnerOuter
    local a_redeclareKeywords::Option{Absyn.RedeclareKeywords}
    local a_finalPrefix::Bool
    local l_cc__str::Tpl.Text
    local l_ec__str::Tpl.Text
    local l_io__str::Tpl.Text
    local l_repl__str::Tpl.Text
    local l_redecl__str::Tpl.Text
    local l_final__str::Tpl.Text
    @match (
      in_txt,
      in_mArg,
      in_a_constrainClass,
      in_a_options,
      in_a_specification,
      in_a_innerOuter,
      in_a_redeclareKeywords,
      in_a_finalPrefix,
    ) begin
      (txt, false, _, _, _, _, _, _) => begin
        txt
      end

      (
        txt,
        _,
        a_constrainClass,
        a_options,
        a_specification,
        a_innerOuter,
        a_redeclareKeywords,
        a_finalPrefix,
      ) => begin
        @assign l_final__str = dumpFinal(Tpl.emptyTxt, a_finalPrefix)
        @assign l_redecl__str = fun_65(Tpl.emptyTxt, a_redeclareKeywords)
        @assign l_repl__str = fun_66(Tpl.emptyTxt, a_redeclareKeywords)
        @assign l_io__str = dumpInnerOuter(Tpl.emptyTxt, a_innerOuter)
        @assign l_ec__str = dumpElementSpec(
          Tpl.emptyTxt,
          a_specification,
          Tpl.textString(l_final__str),
          Tpl.textString(l_redecl__str),
          Tpl.textString(l_repl__str),
          Tpl.textString(l_io__str),
          a_options,
        )
        @assign l_cc__str = fun_67(Tpl.emptyTxt, a_constrainClass)
        @assign txt = Tpl.writeText(txt, l_ec__str)
        @assign txt = Tpl.writeText(txt, l_cc__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(";"))
        txt
      end
    end
  end
  return out_txt
end

function lm_69(in_txt::Tpl.Text, in_items::List{<:Absyn.NamedArg})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Absyn.NamedArg}
    local i_arg::Absyn.NamedArg
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_arg <| rest) => begin
        @assign txt = dumpNamedArg(txt, i_arg)
        @assign txt = lm_69(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function fun_70(in_txt::Tpl.Text, in_a_args::List{<:Absyn.NamedArg})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_args::List{Absyn.NamedArg}
    @match (in_txt, in_a_args) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_args) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("("))
        @assign txt = lm_69(txt, i_args)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(")"))
        txt
      end
    end
  end
  return out_txt
end

function fun_71(in_txt::Tpl.Text, in_a_optName::Option{<:Absyn.Ident})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_name::Absyn.Ident
    @match (in_txt, in_a_optName) begin
      (txt, SOME(i_name)) => begin
        @assign txt = Tpl.writeStr(txt, i_name)
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function fun_72(
  in_txt::Tpl.Text,
  in_mArg::Bool,
  in_a_string::String,
  in_a_info::SourceInfo,
  in_a_optName::Option{<:Absyn.Ident},
)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local a_string::String
    local a_info::SourceInfo
    local a_optName::Option{Absyn.Ident}
    local l_info__str::Tpl.Text
    local l_name__str::Tpl.Text
    @match (in_txt, in_mArg, in_a_string, in_a_info, in_a_optName) begin
      (txt, false, _, _, _) => begin
        txt
      end

      (txt, _, a_string, a_info, a_optName) => begin
        @assign l_name__str = fun_71(Tpl.emptyTxt, a_optName)
        @assign l_info__str = dumpInfo(Tpl.emptyTxt, a_info)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("/* Absyn.TEXT(SOME(\\"))
        @assign txt = Tpl.writeText(txt, l_name__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("\\), \\"))
        @assign txt = Tpl.writeStr(txt, a_string)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("\\, \\"))
        @assign txt = Tpl.writeText(txt, l_info__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("\\); */"))
        txt
      end
    end
  end
  return out_txt
end

function dumpElement(
  in_txt::Tpl.Text,
  in_a_elem::Absyn.Element,
  in_a_options::Dump.DumpOptions,
)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local a_options::Dump.DumpOptions
    local i_string::String
    local i_optName::Option{Absyn.Ident}
    local i_name::Absyn.Ident
    local i_args::List{Absyn.NamedArg}
    local i_constrainClass::Option{Absyn.ConstrainClass}
    local i_specification::Absyn.ElementSpec
    local i_innerOuter::Absyn.InnerOuter
    local i_redeclareKeywords::Option{Absyn.RedeclareKeywords}
    local i_finalPrefix::Bool
    local i_elem::Absyn.Element
    local i_info::SourceInfo
    local ret_5::Bool
    local l_args__str::Tpl.Text
    local ret_3::Bool
    local ret_2::Bool
    local ret_1::Bool
    local ret_0::Bool
    @match (in_txt, in_a_elem, in_a_options) begin
      (
        txt,
        i_elem && Absyn.ELEMENT(
          info = i_info,
          finalPrefix = i_finalPrefix,
          redeclareKeywords = i_redeclareKeywords,
          innerOuter = i_innerOuter,
          specification = i_specification,
          constrainClass = i_constrainClass,
        ),
        a_options,
      ) => begin
        @assign ret_0 = Dump.boolUnparseFileFromInfo(i_info, a_options)
        @assign ret_1 = AbsynUtil.isClassdef(i_elem)
        @assign ret_2 = boolNot(ret_1)
        @assign ret_3 = boolOr(ret_0, ret_2)
        @assign txt = fun_68(
          txt,
          ret_3,
          i_constrainClass,
          a_options,
          i_specification,
          i_innerOuter,
          i_redeclareKeywords,
          i_finalPrefix,
        )
        txt
      end

      (txt, Absyn.DEFINEUNIT(args = i_args, name = i_name), _) => begin
        @assign l_args__str = fun_70(Tpl.emptyTxt, i_args)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("defineunit "))
        @assign txt = Tpl.writeStr(txt, i_name)
        @assign txt = Tpl.writeText(txt, l_args__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(";"))
        txt
      end

      (txt, Absyn.TEXT(info = i_info, optName = i_optName, string = i_string), a_options) => begin
        @assign ret_5 = Dump.boolUnparseFileFromInfo(i_info, a_options)
        @assign txt = fun_72(txt, ret_5, i_string, i_info, i_optName)
        txt
      end

      (txt, _, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function fun_74(in_txt::Tpl.Text, in_a_isReadOnly::Bool)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    @match (in_txt, in_a_isReadOnly) begin
      (txt, false) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("writable"))
        txt
      end

      (txt, _) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("readonly"))
        txt
      end
    end
  end
  return out_txt
end

function dumpInfo(in_txt::Tpl.Text, in_a_info::SourceInfo)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_columnNumberEnd::Integer
    local i_lineNumberEnd::Integer
    local i_columnNumberStart::Integer
    local i_lineNumberStart::Integer
    local i_fileName::String
    local i_isReadOnly::Bool
    local l_rm__str::Tpl.Text
    @match (in_txt, in_a_info) begin
      (
        txt,
        SOURCEINFO(
          isReadOnly = i_isReadOnly,
          fileName = i_fileName,
          lineNumberStart = i_lineNumberStart,
          columnNumberStart = i_columnNumberStart,
          lineNumberEnd = i_lineNumberEnd,
          columnNumberEnd = i_columnNumberEnd,
        ),
      ) => begin
        @assign l_rm__str = fun_74(Tpl.emptyTxt, i_isReadOnly)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("SOURCEINFO(\\"))
        @assign txt = Tpl.writeStr(txt, i_fileName)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("\\, "))
        @assign txt = Tpl.writeText(txt, l_rm__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(", "))
        @assign txt = Tpl.writeStr(txt, intString(i_lineNumberStart))
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(", "))
        @assign txt = Tpl.writeStr(txt, intString(i_columnNumberStart))
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(", "))
        @assign txt = Tpl.writeStr(txt, intString(i_lineNumberEnd))
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(", "))
        @assign txt = Tpl.writeStr(txt, intString(i_columnNumberEnd))
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(")\\\\n"))
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function lm_76(in_txt::Tpl.Text, in_items::List{<:Absyn.ElementArg})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Absyn.ElementArg}
    local i_earg::Absyn.ElementArg
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_earg <| rest) => begin
        @assign txt = dumpElementArg(txt, i_earg)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_76(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function dumpAnnotation(in_txt::Tpl.Text, in_a_ann::Absyn.Annotation)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_elementArgs::List{Absyn.ElementArg}
    local ret_1::Tpl.StringToken
    local txt_0::Tpl.Text
    @match (in_txt, in_a_ann) begin
      (txt, Absyn.ANNOTATION(elementArgs = nil())) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("annotation()"))
        txt
      end

      (txt, Absyn.ANNOTATION(elementArgs = i_elementArgs)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_LINE("annotation(\\n"))
        @assign txt = Tpl.pushBlock(txt, Tpl.BT_INDENT(2))
        @assign txt_0 = Tpl.writeTok(Tpl.emptyTxt, Tpl.ST_STRING(","))
        @assign txt_0 = Tpl.writeTok(txt_0, Tpl.ST_NEW_LINE())
        @assign ret_1 = Tpl.textStrTok(txt_0)
        @assign txt = Tpl.pushIter(
          txt,
          Tpl.ITER_OPTIONS(
            0,
            NONE(),
            SOME(ret_1),
            0,
            0,
            Tpl.ST_NEW_LINE(),
            0,
            Tpl.ST_NEW_LINE(),
          ),
        )
        @assign txt = lm_76(txt, i_elementArgs)
        @assign txt = Tpl.popIter(txt)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(")"))
        @assign txt = Tpl.popBlock(txt)
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function dumpAnnotationOpt(
  in_txt::Tpl.Text,
  in_a_oann::Option{<:Absyn.Annotation},
)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_ann::Absyn.Annotation
    @match (in_txt, in_a_oann) begin
      (txt, SOME(i_ann)) => begin
        @assign txt = dumpAnnotation(txt, i_ann)
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function dumpAnnotationOptSpace(
  in_txt::Tpl.Text,
  in_a_oann::Option{<:Absyn.Annotation},
)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_ann::Absyn.Annotation
    @match (in_txt, in_a_oann) begin
      (txt, SOME(i_ann)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(" "))
        @assign txt = dumpAnnotation(txt, i_ann)
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function dumpComment(in_txt::Tpl.Text, in_a_cmt::Absyn.Comment)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_annotation__::Option{Absyn.Annotation}
    local i_comment::Option{String}
    @match (in_txt, in_a_cmt) begin
      (txt, Absyn.COMMENT(comment = i_comment, annotation_ = i_annotation__)) => begin
        @assign txt = dumpStringCommentOption(txt, i_comment)
        @assign txt = dumpAnnotationOptSpace(txt, i_annotation__)
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function dumpCommentOpt(in_txt::Tpl.Text, in_a_ocmt::Option{<:Absyn.Comment})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_cmt::Absyn.Comment
    @match (in_txt, in_a_ocmt) begin
      (txt, SOME(i_cmt)) => begin
        @assign txt = dumpComment(txt, i_cmt)
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function fun_82(in_txt::Tpl.Text, in_a_modification::Option{<:Absyn.Modification})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_mod::Absyn.Modification
    @match (in_txt, in_a_modification) begin
      (txt, SOME(i_mod)) => begin
        @assign txt = dumpModification(txt, i_mod)
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function fun_83(
  in_txt::Tpl.Text,
  in_a_constrainClass::Option{<:Absyn.ConstrainClass},
)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_cc::Absyn.ConstrainClass
    @match (in_txt, in_a_constrainClass) begin
      (txt, SOME(i_cc)) => begin
        @assign txt = dumpConstrainClass(txt, i_cc)
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function dumpElementArg(in_txt::Tpl.Text, in_a_earg::Absyn.ElementArg)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_constrainClass::Option{Absyn.ConstrainClass}
    local i_elementSpec::Absyn.ElementSpec
    local i_redeclareKeywords::Absyn.RedeclareKeywords
    local i_comment::Option{String}
    local i_modification::Option{Absyn.Modification}
    local i_path::Absyn.Path
    local i_finalPrefix::Bool
    local i_eachPrefix::Absyn.Each
    local l_cc__str::Tpl.Text
    local l_elem__str::Tpl.Text
    local l_eredecl__str::Tpl.Text
    local l_repl__str::Tpl.Text
    local l_redecl__str::Tpl.Text
    local l_cmt__str::Tpl.Text
    local l_mod__str::Tpl.Text
    local l_path__str::Tpl.Text
    local l_final__str::Tpl.Text
    local l_each__str::Tpl.Text
    @match (in_txt, in_a_earg) begin
      (
        txt,
        Absyn.MODIFICATION(
          eachPrefix = i_eachPrefix,
          finalPrefix = i_finalPrefix,
          path = i_path,
          modification = i_modification,
          comment = i_comment,
        ),
      ) => begin
        @assign l_each__str = dumpEach(Tpl.emptyTxt, i_eachPrefix)
        @assign l_final__str = dumpFinal(Tpl.emptyTxt, i_finalPrefix)
        @assign l_path__str = dumpPath(Tpl.emptyTxt, i_path)
        @assign l_mod__str = fun_82(Tpl.emptyTxt, i_modification)
        @assign l_cmt__str = dumpStringCommentOption(Tpl.emptyTxt, i_comment)
        @assign txt = Tpl.writeText(txt, l_each__str)
        @assign txt = Tpl.writeText(txt, l_final__str)
        @assign txt = Tpl.writeText(txt, l_path__str)
        @assign txt = Tpl.writeText(txt, l_mod__str)
        @assign txt = Tpl.writeText(txt, l_cmt__str)
        txt
      end

      (
        txt,
        Absyn.REDECLARATION(
          eachPrefix = i_eachPrefix,
          finalPrefix = i_finalPrefix,
          redeclareKeywords = i_redeclareKeywords,
          elementSpec = i_elementSpec,
          constrainClass = i_constrainClass,
        ),
      ) => begin
        @assign l_each__str = dumpEach(Tpl.emptyTxt, i_eachPrefix)
        @assign l_final__str = dumpFinal(Tpl.emptyTxt, i_finalPrefix)
        @assign l_redecl__str = dumpRedeclare(Tpl.emptyTxt, i_redeclareKeywords)
        @assign l_repl__str = dumpReplaceable(Tpl.emptyTxt, i_redeclareKeywords)
        @assign l_eredecl__str = Tpl.writeText(Tpl.emptyTxt, l_redecl__str)
        @assign l_eredecl__str = Tpl.writeText(l_eredecl__str, l_each__str)
        @assign l_elem__str = dumpElementSpec(
          Tpl.emptyTxt,
          i_elementSpec,
          Tpl.textString(l_final__str),
          Tpl.textString(l_eredecl__str),
          Tpl.textString(l_repl__str),
          "",
          Dump.defaultDumpOptions,
        )
        @assign l_cc__str = fun_83(Tpl.emptyTxt, i_constrainClass)
        @assign txt = Tpl.writeText(txt, l_elem__str)
        @assign txt = Tpl.writeText(txt, l_cc__str)
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function dumpEach(in_txt::Tpl.Text, in_a_each::Absyn.Each)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    @match (in_txt, in_a_each) begin
      (txt, Absyn.EACH(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("each "))
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function dumpFinal(in_txt::Tpl.Text, in_a_final::Bool)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    @match (in_txt, in_a_final) begin
      (txt, false) => begin
        txt
      end

      (txt, _) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("final "))
        txt
      end
    end
  end
  return out_txt
end

function dumpRedeclare(in_txt::Tpl.Text, in_a_redecl::Absyn.RedeclareKeywords)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    @match (in_txt, in_a_redecl) begin
      (txt, Absyn.REDECLARE(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("redeclare "))
        txt
      end

      (txt, Absyn.REDECLARE_REPLACEABLE(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("redeclare "))
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function dumpReplaceable(in_txt::Tpl.Text, in_a_repl::Absyn.RedeclareKeywords)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    @match (in_txt, in_a_repl) begin
      (txt, Absyn.REPLACEABLE(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("replaceable "))
        txt
      end

      (txt, Absyn.REDECLARE_REPLACEABLE(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("replaceable "))
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function dumpInnerOuter(in_txt::Tpl.Text, in_a_io::Absyn.InnerOuter)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    @match (in_txt, in_a_io) begin
      (txt, Absyn.INNER(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("inner "))
        txt
      end

      (txt, Absyn.OUTER(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("outer "))
        txt
      end

      (txt, Absyn.INNER_OUTER(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("inner outer "))
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function lm_90(in_txt::Tpl.Text, in_items::List{<:Absyn.ElementArg})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Absyn.ElementArg}
    local i_earg::Absyn.ElementArg
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_earg <| rest) => begin
        @assign txt = dumpElementArg(txt, i_earg)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_90(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function fun_91(in_txt::Tpl.Text, in_a_elementArgLst::List{<:Absyn.ElementArg})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_elementArgLst::List{Absyn.ElementArg}
    @match (in_txt, in_a_elementArgLst) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_elementArgLst) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("("))
        @assign txt = Tpl.pushIter(
          txt,
          Tpl.ITER_OPTIONS(
            0,
            NONE(),
            SOME(Tpl.ST_STRING(", ")),
            0,
            0,
            Tpl.ST_NEW_LINE(),
            0,
            Tpl.ST_NEW_LINE(),
          ),
        )
        @assign txt = lm_90(txt, i_elementArgLst)
        @assign txt = Tpl.popIter(txt)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(")"))
        txt
      end
    end
  end
  return out_txt
end

function dumpModification(in_txt::Tpl.Text, in_a_mod::Absyn.Modification)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_eqMod::Absyn.EqMod
    local i_elementArgLst::List{Absyn.ElementArg}
    local l_eq__str::Tpl.Text
    local l_arg__str::Tpl.Text
    @match (in_txt, in_a_mod) begin
      (txt, Absyn.CLASSMOD(elementArgLst = i_elementArgLst, eqMod = i_eqMod)) => begin
        @assign l_arg__str = fun_91(Tpl.emptyTxt, i_elementArgLst)
        @assign l_eq__str = dumpEqMod(Tpl.emptyTxt, i_eqMod)
        @assign txt = Tpl.writeText(txt, l_arg__str)
        @assign txt = Tpl.writeText(txt, l_eq__str)
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function dumpEqMod(in_txt::Tpl.Text, in_a_eqmod::Absyn.EqMod)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_exp::Absyn.Exp
    @match (in_txt, in_a_eqmod) begin
      (txt, Absyn.EQMOD(exp = i_exp)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(" "))
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("= "))
        @assign txt = dumpExp(txt, i_exp)
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function lm_94(in_txt::Tpl.Text, in_items::List{<:Absyn.ElementArg})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Absyn.ElementArg}
    local i_earg::Absyn.ElementArg
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_earg <| rest) => begin
        @assign txt = dumpElementArg(txt, i_earg)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_94(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function fun_95(in_txt::Tpl.Text, in_a_args__str::Tpl.Text)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_args__str::Tpl.Text
    @match (in_txt, in_a_args__str) begin
      (txt, Tpl.MEM_TEXT(tokens = nil())) => begin
        txt
      end

      (txt, i_args__str) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("("))
        @assign txt = Tpl.writeText(txt, i_args__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(")"))
        txt
      end
    end
  end
  return out_txt
end

function lm_96(in_txt::Tpl.Text, in_items::List{<:Absyn.ComponentItem})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Absyn.ComponentItem}
    local i_comp::Absyn.ComponentItem
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_comp <| rest) => begin
        @assign txt = dumpComponentItem(txt, i_comp)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_96(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function dumpElementSpec(
  in_txt::Tpl.Text,
  in_a_elem::Absyn.ElementSpec,
  in_a_final::String,
  in_a_redecl::String,
  in_a_repl::String,
  in_a_io::String,
  in_a_options::Dump.DumpOptions,
)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local a_final::String
    local a_redecl::String
    local a_repl::String
    local a_io::String
    local a_options::Dump.DumpOptions
    local i_import__::Absyn.Import
    local i_components::List{Absyn.ComponentItem}
    local i_attributes::Absyn.ElementAttributes
    local i_typeSpec::Absyn.TypeSpec
    local i_annotationOpt::Option{Absyn.Annotation}
    local i_elementArg::List{Absyn.ElementArg}
    local i_path::Absyn.Path
    local i_class__::Absyn.Class
    local l_imp__str::Tpl.Text
    local l_prefix__str::Tpl.Text
    local l_comps__str::Tpl.Text
    local l_dim__str::Tpl.Text
    local l_attr__str::Tpl.Text
    local l_ty__str::Tpl.Text
    local l_ann__str::Tpl.Text
    local l_mod__str::Tpl.Text
    local l_args__str::Tpl.Text
    local l_bc__str::Tpl.Text
    @match (in_txt, in_a_elem, in_a_final, in_a_redecl, in_a_repl, in_a_io, in_a_options) begin
      (txt, Absyn.CLASSDEF(class_ = i_class__), a_final, a_redecl, a_repl, a_io, a_options) => begin
        @assign txt =
          dumpClassElement(txt, i_class__, a_final, a_redecl, a_repl, a_io, a_options)
        txt
      end

      (
        txt,
        Absyn.EXTENDS(
          path = i_path,
          elementArg = i_elementArg,
          annotationOpt = i_annotationOpt,
        ),
        _,
        _,
        _,
        _,
        _,
      ) => begin
        @assign l_bc__str = dumpPath(Tpl.emptyTxt, i_path)
        @assign l_args__str = Tpl.pushIter(
          Tpl.emptyTxt,
          Tpl.ITER_OPTIONS(
            0,
            NONE(),
            SOME(Tpl.ST_STRING(", ")),
            0,
            0,
            Tpl.ST_NEW_LINE(),
            0,
            Tpl.ST_NEW_LINE(),
          ),
        )
        @assign l_args__str = lm_94(l_args__str, i_elementArg)
        @assign l_args__str = Tpl.popIter(l_args__str)
        @assign l_mod__str = fun_95(Tpl.emptyTxt, l_args__str)
        @assign l_ann__str = dumpAnnotationOptSpace(Tpl.emptyTxt, i_annotationOpt)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("extends "))
        @assign txt = Tpl.writeText(txt, l_bc__str)
        @assign txt = Tpl.writeText(txt, l_mod__str)
        @assign txt = Tpl.writeText(txt, l_ann__str)
        txt
      end

      (
        txt,
        Absyn.COMPONENTS(
          typeSpec = i_typeSpec,
          attributes = i_attributes,
          components = i_components,
        ),
        a_final,
        a_redecl,
        a_repl,
        a_io,
        _,
      ) => begin
        @assign l_ty__str = dumpTypeSpec(Tpl.emptyTxt, i_typeSpec)
        @assign l_attr__str = dumpElementAttr(Tpl.emptyTxt, i_attributes)
        @assign l_dim__str = dumpElementAttrDim(Tpl.emptyTxt, i_attributes)
        @assign l_comps__str = Tpl.pushIter(
          Tpl.emptyTxt,
          Tpl.ITER_OPTIONS(
            0,
            NONE(),
            SOME(Tpl.ST_STRING(", ")),
            0,
            0,
            Tpl.ST_NEW_LINE(),
            0,
            Tpl.ST_NEW_LINE(),
          ),
        )
        @assign l_comps__str = lm_96(l_comps__str, i_components)
        @assign l_comps__str = Tpl.popIter(l_comps__str)
        @assign l_prefix__str = Tpl.writeStr(Tpl.emptyTxt, a_redecl)
        @assign l_prefix__str = Tpl.writeStr(l_prefix__str, a_final)
        @assign l_prefix__str = Tpl.writeStr(l_prefix__str, a_io)
        @assign l_prefix__str = Tpl.writeStr(l_prefix__str, a_repl)
        @assign txt = Tpl.writeText(txt, l_prefix__str)
        @assign txt = Tpl.writeText(txt, l_attr__str)
        @assign txt = Tpl.writeText(txt, l_ty__str)
        @assign txt = Tpl.writeText(txt, l_dim__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(" "))
        @assign txt = Tpl.writeText(txt, l_comps__str)
        txt
      end

      (txt, Absyn.IMPORT(import_ = i_import__), _, _, _, _, _) => begin
        @assign l_imp__str = dumpImport(Tpl.emptyTxt, i_import__)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("import "))
        @assign txt = Tpl.writeText(txt, l_imp__str)
        txt
      end

      (txt, _, _, _, _, _, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function fun_98(in_txt::Tpl.Text, in_a_flowPrefix::Bool)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    @match (in_txt, in_a_flowPrefix) begin
      (txt, false) => begin
        txt
      end

      (txt, _) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("flow "))
        txt
      end
    end
  end
  return out_txt
end

function fun_99(in_txt::Tpl.Text, in_a_streamPrefix::Bool)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    @match (in_txt, in_a_streamPrefix) begin
      (txt, false) => begin
        txt
      end

      (txt, _) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("stream "))
        txt
      end
    end
  end
  return out_txt
end

function dumpElementAttr(in_txt::Tpl.Text, in_a_attr::Absyn.ElementAttributes)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_direction::Absyn.Direction
    local i_variability::Absyn.Variability
    local i_isField::Absyn.IsField
    local i_parallelism::Absyn.Parallelism
    local i_streamPrefix::Bool
    local i_flowPrefix::Bool
    local l_dir__str::Tpl.Text
    local l_var__str::Tpl.Text
    local l_field__str::Tpl.Text
    local l_par__str::Tpl.Text
    local l_stream__str::Tpl.Text
    local l_flow__str::Tpl.Text
    @match (in_txt, in_a_attr) begin
      (
        txt,
        Absyn.ATTR(
          flowPrefix = i_flowPrefix,
          streamPrefix = i_streamPrefix,
          parallelism = i_parallelism,
          isField = i_isField,
          variability = i_variability,
          direction = i_direction,
        ),
      ) => begin
        @assign l_flow__str = fun_98(Tpl.emptyTxt, i_flowPrefix)
        @assign l_stream__str = fun_99(Tpl.emptyTxt, i_streamPrefix)
        @assign l_par__str = dumpParallelism(Tpl.emptyTxt, i_parallelism)
        @assign l_field__str = dumpIsField(Tpl.emptyTxt, i_isField)
        @assign l_var__str = dumpVariability(Tpl.emptyTxt, i_variability)
        @assign l_dir__str = dumpDirection(Tpl.emptyTxt, i_direction)
        @assign txt = Tpl.writeText(txt, l_flow__str)
        @assign txt = Tpl.writeText(txt, l_stream__str)
        @assign txt = Tpl.writeText(txt, l_par__str)
        @assign txt = Tpl.writeText(txt, l_field__str)
        @assign txt = Tpl.writeText(txt, l_var__str)
        @assign txt = Tpl.writeText(txt, l_dir__str)
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function dumpParallelism(in_txt::Tpl.Text, in_a_par::Absyn.Parallelism)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    @match (in_txt, in_a_par) begin
      (txt, Absyn.PARGLOBAL(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("parglobal "))
        txt
      end

      (txt, Absyn.PARLOCAL(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("parlocal "))
        txt
      end

      (txt, Absyn.NON_PARALLEL(__)) => begin
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function dumpIsField(in_txt::Tpl.Text, in_a_isField::Absyn.IsField)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    @match (in_txt, in_a_isField) begin
      (txt, Absyn.NONFIELD(__)) => begin
        txt
      end

      (txt, Absyn.FIELD(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("field "))
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function dumpVariability(in_txt::Tpl.Text, in_a_var::Absyn.Variability)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    @match (in_txt, in_a_var) begin
      (txt, Absyn.VAR(__)) => begin
        txt
      end

      (txt, Absyn.DISCRETE(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("discrete "))
        txt
      end

      (txt, Absyn.PARAM(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("parameter "))
        txt
      end

      (txt, Absyn.CONST(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("constant "))
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function dumpDirection(in_txt::Tpl.Text, in_a_dir::Absyn.Direction)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    @match (in_txt, in_a_dir) begin
      (txt, Absyn.BIDIR(__)) => begin
        txt
      end

      (txt, Absyn.INPUT(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("input "))
        txt
      end

      (txt, Absyn.OUTPUT(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("output "))
        txt
      end

      (txt, Absyn.INPUT_OUTPUT(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("input output "))
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function dumpElementAttrDim(in_txt::Tpl.Text, in_a_attr::Absyn.ElementAttributes)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_arrayDim::Absyn.ArrayDim
    @match (in_txt, in_a_attr) begin
      (txt, Absyn.ATTR(arrayDim = i_arrayDim)) => begin
        @assign txt = dumpSubscripts(txt, i_arrayDim)
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function lm_106(in_txt::Tpl.Text, in_items::List{<:Absyn.ElementArg})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Absyn.ElementArg}
    local i_e::Absyn.ElementArg
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_e <| rest) => begin
        @assign txt = dumpElementArg(txt, i_e)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_106(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function fun_107(in_txt::Tpl.Text, in_a_el::List{<:Absyn.ElementArg})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_el::List{Absyn.ElementArg}
    @match (in_txt, in_a_el) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_el) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("("))
        @assign txt = Tpl.pushIter(
          txt,
          Tpl.ITER_OPTIONS(
            0,
            NONE(),
            SOME(Tpl.ST_STRING(", ")),
            0,
            0,
            Tpl.ST_NEW_LINE(),
            0,
            Tpl.ST_NEW_LINE(),
          ),
        )
        @assign txt = lm_106(txt, i_el)
        @assign txt = Tpl.popIter(txt)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(")"))
        txt
      end
    end
  end
  return out_txt
end

function dumpConstrainClass(in_txt::Tpl.Text, in_a_cc::Absyn.ConstrainClass)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_comment::Option{Absyn.Comment}
    local i_el::List{Absyn.ElementArg}
    local i_p::Absyn.Path
    local l_cmt__str::Tpl.Text
    local l_el__str::Tpl.Text
    local l_path__str::Tpl.Text
    @match (in_txt, in_a_cc) begin
      (
        txt,
        Absyn.CONSTRAINCLASS(
          elementSpec = Absyn.EXTENDS(path = i_p, elementArg = i_el),
          comment = i_comment,
        ),
      ) => begin
        @assign l_path__str = dumpPath(Tpl.emptyTxt, i_p)
        @assign l_el__str = fun_107(Tpl.emptyTxt, i_el)
        @assign l_cmt__str = dumpCommentOpt(Tpl.emptyTxt, i_comment)
        @assign txt = Tpl.pushBlock(txt, Tpl.BT_INDENT(1))
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("constrainedby "))
        @assign txt = Tpl.writeText(txt, l_path__str)
        @assign txt = Tpl.writeText(txt, l_el__str)
        @assign txt = Tpl.writeText(txt, l_cmt__str)
        @assign txt = Tpl.popBlock(txt)
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function dumpComponentItem(in_txt::Tpl.Text, in_a_comp::Absyn.ComponentItem)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_comment::Option{Absyn.Comment}
    local i_condition::Option{Absyn.ComponentCondition}
    local i_component::Absyn.Component
    local l_cmt::Tpl.Text
    local l_cond__str::Tpl.Text
    local l_comp__str::Tpl.Text
    @match (in_txt, in_a_comp) begin
      (
        txt,
        Absyn.COMPONENTITEM(
          component = i_component,
          condition = i_condition,
          comment = i_comment,
        ),
      ) => begin
        @assign l_comp__str = dumpComponent(Tpl.emptyTxt, i_component)
        @assign l_cond__str = dumpComponentCondition(Tpl.emptyTxt, i_condition)
        @assign l_cmt = dumpCommentOpt(Tpl.emptyTxt, i_comment)
        @assign txt = Tpl.writeText(txt, l_comp__str)
        @assign txt = Tpl.writeText(txt, l_cond__str)
        @assign txt = Tpl.writeText(txt, l_cmt)
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function fun_110(
  in_txt::Tpl.Text,
  in_a_modification::Option{<:Absyn.Modification},
)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_mod::Absyn.Modification
    @match (in_txt, in_a_modification) begin
      (txt, SOME(i_mod)) => begin
        @assign txt = dumpModification(txt, i_mod)
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function dumpComponent(in_txt::Tpl.Text, in_a_comp::Absyn.Component)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_name::Absyn.Ident
    local i_modification::Option{Absyn.Modification}
    local i_arrayDim::Absyn.ArrayDim
    local l_mod__str::Tpl.Text
    local l_dim__str::Tpl.Text
    @match (in_txt, in_a_comp) begin
      (
        txt,
        Absyn.COMPONENT(
          arrayDim = i_arrayDim,
          modification = i_modification,
          name = i_name,
        ),
      ) => begin
        @assign l_dim__str = dumpSubscripts(Tpl.emptyTxt, i_arrayDim)
        @assign l_mod__str = fun_110(Tpl.emptyTxt, i_modification)
        @assign txt = Tpl.writeStr(txt, i_name)
        @assign txt = Tpl.writeText(txt, l_dim__str)
        @assign txt = Tpl.writeText(txt, l_mod__str)
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function dumpComponentCondition(
  in_txt::Tpl.Text,
  in_a_cond::Option{<:Absyn.ComponentCondition},
)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_cexp::Absyn.ComponentCondition
    local l_exp__str::Tpl.Text
    @match (in_txt, in_a_cond) begin
      (txt, SOME(i_cexp)) => begin
        @assign l_exp__str = dumpExp(Tpl.emptyTxt, i_cexp)
        @assign txt = Tpl.pushBlock(txt, Tpl.BT_INDENT(1))
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("if "))
        @assign txt = Tpl.writeText(txt, l_exp__str)
        @assign txt = Tpl.popBlock(txt)
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function lm_113(in_txt::Tpl.Text, in_items::List{<:Absyn.GroupImport})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Absyn.GroupImport}
    local i_group::Absyn.GroupImport
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_group <| rest) => begin
        @assign txt = dumpGroupImport(txt, i_group)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_113(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function dumpImport(in_txt::Tpl.Text, in_a_imp::Absyn.Import)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_groups::List{Absyn.GroupImport}
    local i_prefix::Absyn.Path
    local i_path::Absyn.Path
    local i_name::Absyn.Ident
    local l_groups__str::Tpl.Text
    local l_prefix__str::Tpl.Text
    @match (in_txt, in_a_imp) begin
      (txt, Absyn.NAMED_IMPORT(name = i_name, path = i_path)) => begin
        @assign txt = Tpl.writeStr(txt, i_name)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(" = "))
        @assign txt = dumpPath(txt, i_path)
        txt
      end

      (txt, Absyn.QUAL_IMPORT(path = i_path)) => begin
        @assign txt = dumpPath(txt, i_path)
        txt
      end

      (txt, Absyn.UNQUAL_IMPORT(path = i_path)) => begin
        @assign txt = dumpPath(txt, i_path)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(".*"))
        txt
      end

      (txt, Absyn.GROUP_IMPORT(prefix = i_prefix, groups = i_groups)) => begin
        @assign l_prefix__str = dumpPath(Tpl.emptyTxt, i_prefix)
        @assign l_groups__str = Tpl.pushIter(
          Tpl.emptyTxt,
          Tpl.ITER_OPTIONS(
            0,
            NONE(),
            SOME(Tpl.ST_STRING(",")),
            0,
            0,
            Tpl.ST_NEW_LINE(),
            0,
            Tpl.ST_NEW_LINE(),
          ),
        )
        @assign l_groups__str = lm_113(l_groups__str, i_groups)
        @assign l_groups__str = Tpl.popIter(l_groups__str)
        @assign txt = Tpl.writeText(txt, l_prefix__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(".{"))
        @assign txt = Tpl.writeText(txt, l_groups__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("}"))
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function dumpGroupImport(in_txt::Tpl.Text, in_a_gimp::Absyn.GroupImport)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_rename::String
    local i_name::String
    @match (in_txt, in_a_gimp) begin
      (txt, Absyn.GROUP_IMPORT_NAME(name = i_name)) => begin
        @assign txt = Tpl.writeStr(txt, i_name)
        txt
      end

      (txt, Absyn.GROUP_IMPORT_RENAME(rename = i_rename, name = i_name)) => begin
        @assign txt = Tpl.writeStr(txt, i_rename)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(" = "))
        @assign txt = Tpl.writeStr(txt, i_name)
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function dumpEquationItem(in_txt::Tpl.Text, in_a_eq::Absyn.EquationItem)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_comment_1::String
    local i_comment::Option{Absyn.Comment}
    local i_equation__::Absyn.Equation
    local ret_2::String
    local l_cmt__str::Tpl.Text
    local l_eq__str::Tpl.Text
    @match (in_txt, in_a_eq) begin
      (txt, Absyn.EQUATIONITEM(equation_ = i_equation__, comment = i_comment)) => begin
        @assign l_eq__str = dumpEquation(Tpl.emptyTxt, i_equation__)
        @assign l_cmt__str = dumpCommentOpt(Tpl.emptyTxt, i_comment)
        @assign txt = Tpl.writeText(txt, l_eq__str)
        @assign txt = Tpl.writeText(txt, l_cmt__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(";"))
        txt
      end

      (txt, Absyn.EQUATIONITEMCOMMENT(comment = i_comment_1)) => begin
        @assign txt = Tpl.pushBlock(txt, Tpl.BT_ABS_INDENT(0))
        @assign ret_2 = System.trimWhitespace(i_comment_1)
        @assign txt = Tpl.writeStr(txt, ret_2)
        @assign txt = Tpl.popBlock(txt)
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function lm_117(in_txt::Tpl.Text, in_items::List{<:Absyn.EquationItem})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Absyn.EquationItem}
    local i_eq::Absyn.EquationItem
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_eq <| rest) => begin
        @assign txt = dumpEquationItem(txt, i_eq)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_117(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function dumpEquationItems(txt::Tpl.Text, a_eql::List{<:Absyn.EquationItem})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = Tpl.pushIter(
    txt,
    Tpl.ITER_OPTIONS(
      0,
      NONE(),
      SOME(Tpl.ST_NEW_LINE()),
      0,
      0,
      Tpl.ST_NEW_LINE(),
      0,
      Tpl.ST_NEW_LINE(),
    ),
  )
  @assign out_txt = lm_117(out_txt, a_eql)
  @assign out_txt = Tpl.popIter(out_txt)
  return out_txt
end

function lm_119(
  in_txt::Tpl.Text,
  in_items::List{<:Tuple{<:Absyn.Exp, List{<:Absyn.EquationItem}}},
)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Tuple{Absyn.Exp, List{Absyn.EquationItem}}}
    local i_b::List{Absyn.EquationItem}
    local i_c::Absyn.Exp
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, (i_c, i_b) <| rest) => begin
        @assign txt = dumpEquationBranch(txt, i_c, i_b, "elseif")
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_119(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function fun_120(in_txt::Tpl.Text, in_a_else__branch__str::Tpl.Text)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_else__branch__str::Tpl.Text
    @match (in_txt, in_a_else__branch__str) begin
      (txt, Tpl.MEM_TEXT(tokens = nil())) => begin
        txt
      end

      (txt, i_else__branch__str) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_LINE("else\\n"))
        @assign txt = Tpl.pushBlock(txt, Tpl.BT_INDENT(2))
        @assign txt = Tpl.writeText(txt, i_else__branch__str)
        @assign txt = Tpl.popBlock(txt)
        txt
      end
    end
  end
  return out_txt
end

function lm_121(
  in_txt::Tpl.Text,
  in_items::List{<:Tuple{<:Absyn.Exp, List{<:Absyn.EquationItem}}},
)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Tuple{Absyn.Exp, List{Absyn.EquationItem}}}
    local i_b::List{Absyn.EquationItem}
    local i_c::Absyn.Exp
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, (i_c, i_b) <| rest) => begin
        @assign txt = dumpEquationBranch(txt, i_c, i_b, "elsewhen")
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_121(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function dumpEquation(in_txt::Tpl.Text, in_a_eq::Absyn.Equation)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_equ::Absyn.EquationItem
    local i_functionArgs::Absyn.FunctionArgs
    local i_functionName::Absyn.ComponentRef
    local i_elseWhenEquations::List{Tuple{Absyn.Exp, List{Absyn.EquationItem}}}
    local i_whenEquations::List{Absyn.EquationItem}
    local i_whenExp::Absyn.Exp
    local i_forEquations::List{Absyn.EquationItem}
    local i_iterators::Absyn.ForIterators
    local i_connector2::Absyn.ComponentRef
    local i_connector1::Absyn.ComponentRef
    local i_domain::Absyn.ComponentRef
    local i_rightSide::Absyn.Exp
    local i_leftSide::Absyn.Exp
    local i_equationElseItems::List{Absyn.EquationItem}
    local i_elseIfBranches::List{Tuple{Absyn.Exp, List{Absyn.EquationItem}}}
    local i_equationTrueItems::List{Absyn.EquationItem}
    local i_ifExp::Absyn.Exp
    local l_eq__str::Tpl.Text
    local l_args__str::Tpl.Text
    local l_name__str::Tpl.Text
    local l_elsewhen__str::Tpl.Text
    local l_when__str::Tpl.Text
    local l_body__str::Tpl.Text
    local l_iter__str::Tpl.Text
    local l_c2__str::Tpl.Text
    local l_c1__str::Tpl.Text
    local l_domain__str::Tpl.Text
    local l_rhs::Tpl.Text
    local l_lhs::Tpl.Text
    local l_else__str::Tpl.Text
    local l_else__branch__str::Tpl.Text
    local l_elseif__str::Tpl.Text
    local l_if__str::Tpl.Text
    @match (in_txt, in_a_eq) begin
      (
        txt,
        Absyn.EQ_IF(
          ifExp = i_ifExp,
          equationTrueItems = i_equationTrueItems,
          elseIfBranches = i_elseIfBranches,
          equationElseItems = i_equationElseItems,
        ),
      ) => begin
        @assign l_if__str =
          dumpEquationBranch(Tpl.emptyTxt, i_ifExp, i_equationTrueItems, "if")
        @assign l_elseif__str = Tpl.pushIter(
          Tpl.emptyTxt,
          Tpl.ITER_OPTIONS(
            0,
            NONE(),
            SOME(Tpl.ST_NEW_LINE()),
            0,
            0,
            Tpl.ST_NEW_LINE(),
            0,
            Tpl.ST_NEW_LINE(),
          ),
        )
        @assign l_elseif__str = lm_119(l_elseif__str, i_elseIfBranches)
        @assign l_elseif__str = Tpl.popIter(l_elseif__str)
        @assign l_else__branch__str = dumpEquationItems(Tpl.emptyTxt, i_equationElseItems)
        @assign l_else__str = fun_120(Tpl.emptyTxt, l_else__branch__str)
        @assign txt = Tpl.writeText(txt, l_if__str)
        @assign txt = Tpl.softNewLine(txt)
        @assign txt = Tpl.writeText(txt, l_elseif__str)
        @assign txt = Tpl.softNewLine(txt)
        @assign txt = Tpl.writeText(txt, l_else__str)
        @assign txt = Tpl.softNewLine(txt)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("end if"))
        txt
      end

      (txt, Absyn.EQ_EQUALS(leftSide = i_leftSide, rightSide = i_rightSide)) => begin
        @assign l_lhs = dumpLhsExp(Tpl.emptyTxt, i_leftSide)
        @assign l_rhs = dumpExp(Tpl.emptyTxt, i_rightSide)
        @assign txt = Tpl.writeText(txt, l_lhs)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(" = "))
        @assign txt = Tpl.writeText(txt, l_rhs)
        txt
      end

      (
        txt,
        Absyn.EQ_PDE(leftSide = i_leftSide, rightSide = i_rightSide, domain = i_domain),
      ) => begin
        @assign l_lhs = dumpLhsExp(Tpl.emptyTxt, i_leftSide)
        @assign l_rhs = dumpExp(Tpl.emptyTxt, i_rightSide)
        @assign l_domain__str = dumpCref(Tpl.emptyTxt, i_domain)
        @assign txt = Tpl.writeText(txt, l_lhs)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(" = "))
        @assign txt = Tpl.writeText(txt, l_rhs)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(" indomain "))
        @assign txt = Tpl.writeText(txt, l_domain__str)
        txt
      end

      (txt, Absyn.EQ_CONNECT(connector1 = i_connector1, connector2 = i_connector2)) =>
        begin
          @assign l_c1__str = dumpCref(Tpl.emptyTxt, i_connector1)
          @assign l_c2__str = dumpCref(Tpl.emptyTxt, i_connector2)
          @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("connect("))
          @assign txt = Tpl.writeText(txt, l_c1__str)
          @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(", "))
          @assign txt = Tpl.writeText(txt, l_c2__str)
          @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(")"))
          txt
        end

      (txt, Absyn.EQ_FOR(iterators = i_iterators, forEquations = i_forEquations)) => begin
        @assign l_iter__str = dumpForIterators(Tpl.emptyTxt, i_iterators)
        @assign l_body__str = dumpEquationItems(Tpl.emptyTxt, i_forEquations)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("for "))
        @assign txt = Tpl.writeText(txt, l_iter__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_LINE(" loop\\n"))
        @assign txt = Tpl.pushBlock(txt, Tpl.BT_INDENT(2))
        @assign txt = Tpl.writeText(txt, l_body__str)
        @assign txt = Tpl.softNewLine(txt)
        @assign txt = Tpl.popBlock(txt)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("end for"))
        txt
      end

      (
        txt,
        Absyn.EQ_WHEN_E(
          whenExp = i_whenExp,
          whenEquations = i_whenEquations,
          elseWhenEquations = i_elseWhenEquations,
        ),
      ) => begin
        @assign l_when__str =
          dumpEquationBranch(Tpl.emptyTxt, i_whenExp, i_whenEquations, "when")
        @assign l_elsewhen__str = Tpl.pushIter(
          Tpl.emptyTxt,
          Tpl.ITER_OPTIONS(
            0,
            NONE(),
            SOME(Tpl.ST_NEW_LINE()),
            0,
            0,
            Tpl.ST_NEW_LINE(),
            0,
            Tpl.ST_NEW_LINE(),
          ),
        )
        @assign l_elsewhen__str = lm_121(l_elsewhen__str, i_elseWhenEquations)
        @assign l_elsewhen__str = Tpl.popIter(l_elsewhen__str)
        @assign txt = Tpl.writeText(txt, l_when__str)
        @assign txt = Tpl.softNewLine(txt)
        @assign txt = Tpl.writeText(txt, l_elsewhen__str)
        @assign txt = Tpl.softNewLine(txt)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("end when"))
        txt
      end

      (
        txt,
        Absyn.EQ_NORETCALL(functionName = i_functionName, functionArgs = i_functionArgs),
      ) => begin
        @assign l_name__str = dumpCref(Tpl.emptyTxt, i_functionName)
        @assign l_args__str = dumpFunctionArgs(Tpl.emptyTxt, i_functionArgs)
        @assign txt = Tpl.writeText(txt, l_name__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("("))
        @assign txt = Tpl.writeText(txt, l_args__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(")"))
        txt
      end

      (txt, Absyn.EQ_FAILURE(equ = i_equ)) => begin
        @assign l_eq__str = dumpEquationItem(Tpl.emptyTxt, i_equ)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("failure("))
        @assign txt = Tpl.writeText(txt, l_eq__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(")"))
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function lm_123(in_txt::Tpl.Text, in_items::List{<:Absyn.EquationItem})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Absyn.EquationItem}
    local i_eq::Absyn.EquationItem
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_eq <| rest) => begin
        @assign txt = dumpEquationItem(txt, i_eq)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_123(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function dumpEquationBranch(
  txt::Tpl.Text,
  a_cond::Absyn.Exp,
  a_body::List{<:Absyn.EquationItem},
  a_header::String,
)::Tpl.Text
  local out_txt::Tpl.Text

  local l_body__str::Tpl.Text
  local l_cond__str::Tpl.Text

  @assign l_cond__str = dumpExp(Tpl.emptyTxt, a_cond)
  @assign l_body__str = Tpl.pushIter(
    Tpl.emptyTxt,
    Tpl.ITER_OPTIONS(
      0,
      NONE(),
      SOME(Tpl.ST_NEW_LINE()),
      0,
      0,
      Tpl.ST_NEW_LINE(),
      0,
      Tpl.ST_NEW_LINE(),
    ),
  )
  @assign l_body__str = lm_123(l_body__str, a_body)
  @assign l_body__str = Tpl.popIter(l_body__str)
  @assign out_txt = Tpl.writeStr(txt, a_header)
  @assign out_txt = Tpl.writeTok(out_txt, Tpl.ST_STRING(" "))
  @assign out_txt = Tpl.writeText(out_txt, l_cond__str)
  @assign out_txt = Tpl.writeTok(out_txt, Tpl.ST_LINE(" then\\n"))
  @assign out_txt = Tpl.pushBlock(out_txt, Tpl.BT_INDENT(2))
  @assign out_txt = Tpl.writeText(out_txt, l_body__str)
  @assign out_txt = Tpl.popBlock(out_txt)
  return out_txt
end

function lm_125(in_txt::Tpl.Text, in_items::List{<:Absyn.AlgorithmItem})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Absyn.AlgorithmItem}
    local i_alg::Absyn.AlgorithmItem
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_alg <| rest) => begin
        @assign txt = dumpAlgorithmItem(txt, i_alg)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_125(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function dumpAlgorithmItems(txt::Tpl.Text, a_algs::List{<:Absyn.AlgorithmItem})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = Tpl.pushIter(
    txt,
    Tpl.ITER_OPTIONS(
      0,
      NONE(),
      SOME(Tpl.ST_NEW_LINE()),
      0,
      0,
      Tpl.ST_NEW_LINE(),
      0,
      Tpl.ST_NEW_LINE(),
    ),
  )
  @assign out_txt = lm_125(out_txt, a_algs)
  @assign out_txt = Tpl.popIter(out_txt)
  return out_txt
end

function dumpAlgorithmItem(in_txt::Tpl.Text, in_a_alg::Absyn.AlgorithmItem)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_comment_1::String
    local i_comment::Option{Absyn.Comment}
    local i_algorithm__::Absyn.Algorithm
    local ret_2::String
    local l_cmt__str::Tpl.Text
    local l_alg__str::Tpl.Text
    @match (in_txt, in_a_alg) begin
      (txt, Absyn.ALGORITHMITEM(algorithm_ = i_algorithm__, comment = i_comment)) => begin
        @assign l_alg__str = dumpAlgorithm(Tpl.emptyTxt, i_algorithm__)
        @assign l_cmt__str = dumpCommentOpt(Tpl.emptyTxt, i_comment)
        @assign txt = Tpl.writeText(txt, l_alg__str)
        @assign txt = Tpl.writeText(txt, l_cmt__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(";"))
        txt
      end

      (txt, Absyn.ALGORITHMITEMCOMMENT(comment = i_comment_1)) => begin
        @assign txt = Tpl.pushBlock(txt, Tpl.BT_ABS_INDENT(0))
        @assign ret_2 = System.trimWhitespace(i_comment_1)
        @assign txt = Tpl.writeStr(txt, ret_2)
        @assign txt = Tpl.popBlock(txt)
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function lm_128(
  in_txt::Tpl.Text,
  in_items::List{<:Tuple{<:Absyn.Exp, List{<:Absyn.AlgorithmItem}}},
)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Tuple{Absyn.Exp, List{Absyn.AlgorithmItem}}}
    local i_b::List{Absyn.AlgorithmItem}
    local i_c::Absyn.Exp
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, (i_c, i_b) <| rest) => begin
        @assign txt = dumpAlgorithmBranch(txt, i_c, i_b, "elseif", "then")
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_128(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function fun_129(in_txt::Tpl.Text, in_a_else__branch__str::Tpl.Text)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_else__branch__str::Tpl.Text
    @match (in_txt, in_a_else__branch__str) begin
      (txt, Tpl.MEM_TEXT(tokens = nil())) => begin
        txt
      end

      (txt, i_else__branch__str) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_LINE("else\\n"))
        @assign txt = Tpl.pushBlock(txt, Tpl.BT_INDENT(2))
        @assign txt = Tpl.writeText(txt, i_else__branch__str)
        @assign txt = Tpl.popBlock(txt)
        txt
      end
    end
  end
  return out_txt
end

function lm_130(
  in_txt::Tpl.Text,
  in_items::List{<:Tuple{<:Absyn.Exp, List{<:Absyn.AlgorithmItem}}},
)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Tuple{Absyn.Exp, List{Absyn.AlgorithmItem}}}
    local i_b::List{Absyn.AlgorithmItem}
    local i_c::Absyn.Exp
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, (i_c, i_b) <| rest) => begin
        @assign txt = dumpAlgorithmBranch(txt, i_c, i_b, "elsewhen", "then")
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_130(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function fun_131(in_txt::Tpl.Text, in_a_equ::List{<:Absyn.AlgorithmItem})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_equ::List{Absyn.AlgorithmItem}
    @match (in_txt, in_a_equ) begin
      (txt, nil()) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("..."))
        txt
      end

      (txt, i_equ) => begin
        @assign txt = dumpAlgorithmItems(txt, i_equ)
        txt
      end
    end
  end
  return out_txt
end

function dumpAlgorithm(in_txt::Tpl.Text, in_a_alg::Absyn.Algorithm)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_elseBody::List{Absyn.AlgorithmItem}
    local i_body::List{Absyn.AlgorithmItem}
    local i_equ::List{Absyn.AlgorithmItem}
    local i_functionArgs::Absyn.FunctionArgs
    local i_functionCall::Absyn.ComponentRef
    local i_elseWhenAlgorithmBranch::List{Tuple{Absyn.Exp, List{Absyn.AlgorithmItem}}}
    local i_whenBody::List{Absyn.AlgorithmItem}
    local i_whileBody::List{Absyn.AlgorithmItem}
    local i_boolExpr::Absyn.Exp
    local i_parforBody::List{Absyn.AlgorithmItem}
    local i_forBody::List{Absyn.AlgorithmItem}
    local i_iterators::Absyn.ForIterators
    local i_elseBranch::List{Absyn.AlgorithmItem}
    local i_elseIfAlgorithmBranch::List{Tuple{Absyn.Exp, List{Absyn.AlgorithmItem}}}
    local i_trueBranch::List{Absyn.AlgorithmItem}
    local i_ifExp::Absyn.Exp
    local i_value::Absyn.Exp
    local i_assignComponent::Absyn.Exp
    local l_arg2::Tpl.Text
    local l_arg1::Tpl.Text
    local l_arg__str::Tpl.Text
    local l_args__str::Tpl.Text
    local l_name__str::Tpl.Text
    local l_elsewhen__str::Tpl.Text
    local l_when__str::Tpl.Text
    local l_while__str::Tpl.Text
    local l_body__str::Tpl.Text
    local l_iter__str::Tpl.Text
    local l_else__str::Tpl.Text
    local l_else__branch__str::Tpl.Text
    local l_elseif__str::Tpl.Text
    local l_if__str::Tpl.Text
    local l_rhs__str::Tpl.Text
    local l_lhs__str::Tpl.Text
    @match (in_txt, in_a_alg) begin
      (txt, Absyn.ALG_ASSIGN(assignComponent = i_assignComponent, value = i_value)) =>
        begin
          @assign l_lhs__str = dumpLhsExp(Tpl.emptyTxt, i_assignComponent)
          @assign l_rhs__str = dumpExp(Tpl.emptyTxt, i_value)
          @assign txt = Tpl.writeText(txt, l_lhs__str)
          @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(" := "))
          @assign txt = Tpl.writeText(txt, l_rhs__str)
          txt
        end

      (
        txt,
        Absyn.ALG_IF(
          ifExp = i_ifExp,
          trueBranch = i_trueBranch,
          elseIfAlgorithmBranch = i_elseIfAlgorithmBranch,
          elseBranch = i_elseBranch,
        ),
      ) => begin
        @assign l_if__str =
          dumpAlgorithmBranch(Tpl.emptyTxt, i_ifExp, i_trueBranch, "if", "then")
        @assign l_elseif__str = Tpl.pushIter(
          Tpl.emptyTxt,
          Tpl.ITER_OPTIONS(
            0,
            NONE(),
            SOME(Tpl.ST_NEW_LINE()),
            0,
            0,
            Tpl.ST_NEW_LINE(),
            0,
            Tpl.ST_NEW_LINE(),
          ),
        )
        @assign l_elseif__str = lm_128(l_elseif__str, i_elseIfAlgorithmBranch)
        @assign l_elseif__str = Tpl.popIter(l_elseif__str)
        @assign l_else__branch__str = dumpAlgorithmItems(Tpl.emptyTxt, i_elseBranch)
        @assign l_else__str = fun_129(Tpl.emptyTxt, l_else__branch__str)
        @assign txt = Tpl.writeText(txt, l_if__str)
        @assign txt = Tpl.softNewLine(txt)
        @assign txt = Tpl.writeText(txt, l_elseif__str)
        @assign txt = Tpl.softNewLine(txt)
        @assign txt = Tpl.writeText(txt, l_else__str)
        @assign txt = Tpl.softNewLine(txt)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("end if"))
        txt
      end

      (txt, Absyn.ALG_FOR(iterators = i_iterators, forBody = i_forBody)) => begin
        @assign l_iter__str = dumpForIterators(Tpl.emptyTxt, i_iterators)
        @assign l_body__str = dumpAlgorithmItems(Tpl.emptyTxt, i_forBody)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("for "))
        @assign txt = Tpl.writeText(txt, l_iter__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_LINE(" loop\\n"))
        @assign txt = Tpl.pushBlock(txt, Tpl.BT_INDENT(2))
        @assign txt = Tpl.writeText(txt, l_body__str)
        @assign txt = Tpl.softNewLine(txt)
        @assign txt = Tpl.popBlock(txt)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("end for"))
        txt
      end

      (txt, Absyn.ALG_PARFOR(iterators = i_iterators, parforBody = i_parforBody)) => begin
        @assign l_iter__str = dumpForIterators(Tpl.emptyTxt, i_iterators)
        @assign l_body__str = dumpAlgorithmItems(Tpl.emptyTxt, i_parforBody)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("parfor "))
        @assign txt = Tpl.writeText(txt, l_iter__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_LINE(" loop\\n"))
        @assign txt = Tpl.pushBlock(txt, Tpl.BT_INDENT(2))
        @assign txt = Tpl.writeText(txt, l_body__str)
        @assign txt = Tpl.softNewLine(txt)
        @assign txt = Tpl.popBlock(txt)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("end parfor"))
        txt
      end

      (txt, Absyn.ALG_WHILE(boolExpr = i_boolExpr, whileBody = i_whileBody)) => begin
        @assign l_while__str =
          dumpAlgorithmBranch(Tpl.emptyTxt, i_boolExpr, i_whileBody, "while", "loop")
        @assign txt = Tpl.writeText(txt, l_while__str)
        @assign txt = Tpl.softNewLine(txt)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("end while"))
        txt
      end

      (
        txt,
        Absyn.ALG_WHEN_A(
          boolExpr = i_boolExpr,
          whenBody = i_whenBody,
          elseWhenAlgorithmBranch = i_elseWhenAlgorithmBranch,
        ),
      ) => begin
        @assign l_when__str =
          dumpAlgorithmBranch(Tpl.emptyTxt, i_boolExpr, i_whenBody, "when", "then")
        @assign l_elsewhen__str = Tpl.pushIter(
          Tpl.emptyTxt,
          Tpl.ITER_OPTIONS(
            0,
            NONE(),
            SOME(Tpl.ST_NEW_LINE()),
            0,
            0,
            Tpl.ST_NEW_LINE(),
            0,
            Tpl.ST_NEW_LINE(),
          ),
        )
        @assign l_elsewhen__str = lm_130(l_elsewhen__str, i_elseWhenAlgorithmBranch)
        @assign l_elsewhen__str = Tpl.popIter(l_elsewhen__str)
        @assign txt = Tpl.writeText(txt, l_when__str)
        @assign txt = Tpl.softNewLine(txt)
        @assign txt = Tpl.writeText(txt, l_elsewhen__str)
        @assign txt = Tpl.softNewLine(txt)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("end when"))
        txt
      end

      (
        txt,
        Absyn.ALG_NORETCALL(functionCall = i_functionCall, functionArgs = i_functionArgs),
      ) => begin
        @assign l_name__str = dumpCref(Tpl.emptyTxt, i_functionCall)
        @assign l_args__str = dumpFunctionArgs(Tpl.emptyTxt, i_functionArgs)
        @assign txt = Tpl.writeText(txt, l_name__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("("))
        @assign txt = Tpl.writeText(txt, l_args__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(")"))
        txt
      end

      (txt, Absyn.ALG_RETURN(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("return"))
        txt
      end

      (txt, Absyn.ALG_BREAK(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("break"))
        txt
      end

      (txt, Absyn.ALG_FAILURE(equ = i_equ)) => begin
        @assign l_arg__str = fun_131(Tpl.emptyTxt, i_equ)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("failure("))
        @assign txt = Tpl.writeText(txt, l_arg__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(")"))
        txt
      end

      (txt, Absyn.ALG_TRY(body = i_body, elseBody = i_elseBody)) => begin
        @assign l_arg1 = dumpAlgorithmItems(Tpl.emptyTxt, i_body)
        @assign l_arg2 = dumpAlgorithmItems(Tpl.emptyTxt, i_elseBody)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_LINE("try\\n"))
        @assign txt = Tpl.pushBlock(txt, Tpl.BT_INDENT(2))
        @assign txt = Tpl.writeText(txt, l_arg1)
        @assign txt = Tpl.softNewLine(txt)
        @assign txt = Tpl.popBlock(txt)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_LINE("else\\n"))
        @assign txt = Tpl.pushBlock(txt, Tpl.BT_INDENT(2))
        @assign txt = Tpl.writeText(txt, l_arg2)
        @assign txt = Tpl.softNewLine(txt)
        @assign txt = Tpl.popBlock(txt)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("end try;"))
        txt
      end

      (txt, Absyn.ALG_CONTINUE(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("continue"))
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function lm_133(in_txt::Tpl.Text, in_items::List{<:Absyn.AlgorithmItem})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Absyn.AlgorithmItem}
    local i_eq::Absyn.AlgorithmItem
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_eq <| rest) => begin
        @assign txt = dumpAlgorithmItem(txt, i_eq)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_133(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function dumpAlgorithmBranch(
  txt::Tpl.Text,
  a_cond::Absyn.Exp,
  a_body::List{<:Absyn.AlgorithmItem},
  a_header::String,
  a_exec__str::String,
)::Tpl.Text
  local out_txt::Tpl.Text

  local l_body__str::Tpl.Text
  local l_cond__str::Tpl.Text

  @assign l_cond__str = dumpExp(Tpl.emptyTxt, a_cond)
  @assign l_body__str = Tpl.pushIter(
    Tpl.emptyTxt,
    Tpl.ITER_OPTIONS(
      0,
      NONE(),
      SOME(Tpl.ST_NEW_LINE()),
      0,
      0,
      Tpl.ST_NEW_LINE(),
      0,
      Tpl.ST_NEW_LINE(),
    ),
  )
  @assign l_body__str = lm_133(l_body__str, a_body)
  @assign l_body__str = Tpl.popIter(l_body__str)
  @assign out_txt = Tpl.writeStr(txt, a_header)
  @assign out_txt = Tpl.writeTok(out_txt, Tpl.ST_STRING(" "))
  @assign out_txt = Tpl.writeText(out_txt, l_cond__str)
  @assign out_txt = Tpl.writeTok(out_txt, Tpl.ST_STRING(" "))
  @assign out_txt = Tpl.writeStr(out_txt, a_exec__str)
  @assign out_txt = Tpl.softNewLine(out_txt)
  @assign out_txt = Tpl.pushBlock(out_txt, Tpl.BT_INDENT(2))
  @assign out_txt = Tpl.writeText(out_txt, l_body__str)
  @assign out_txt = Tpl.popBlock(out_txt)
  return out_txt
end

function fun_135(
  in_txt::Tpl.Text,
  in_mArg::Bool,
  in_a_path::Absyn.Path,
  in_a_name::Absyn.Ident,
)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local a_path::Absyn.Path
    local a_name::Absyn.Ident
    @match (in_txt, in_mArg, in_a_path, in_a_name) begin
      (txt, false, a_path, a_name) => begin
        @assign txt = Tpl.writeStr(txt, a_name)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("."))
        @assign txt = dumpPath(txt, a_path)
        txt
      end

      (txt, _, a_path, a_name) => begin
        @assign txt = Tpl.writeStr(txt, a_name)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("__"))
        @assign txt = dumpPath(txt, a_path)
        txt
      end
    end
  end
  return out_txt
end

function dumpPath(in_txt::Tpl.Text, in_a_path::Absyn.Path)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_name::Absyn.Ident
    local i_path::Absyn.Path
    local ret_0::Bool
    @match (in_txt, in_a_path) begin
      (txt, Absyn.FULLYQUALIFIED(path = i_path)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("."))
        @assign txt = dumpPath(txt, i_path)
        txt
      end

      (txt, Absyn.QUALIFIED(name = i_name, path = i_path)) => begin
        @assign ret_0 = Flags.getConfigBool(Flags.MODELICA_OUTPUT)
        @assign txt = fun_135(txt, ret_0, i_path, i_name)
        txt
      end

      (txt, Absyn.IDENT(name = i_name)) => begin
        @assign txt = Tpl.writeStr(txt, i_name)
        txt
      end

      (txt, _) => begin
        @assign txt = errorMsg(txt, "SCodeDump.dumpPath: Unknown path.")
        txt
      end
    end
  end
  return out_txt
end

function dumpPathNoQual(in_txt::Tpl.Text, in_a_path::Absyn.Path)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_path::Absyn.Path
    @match (in_txt, in_a_path) begin
      (txt, Absyn.FULLYQUALIFIED(path = i_path)) => begin
        @assign txt = dumpPath(txt, i_path)
        txt
      end

      (txt, i_path) => begin
        @assign txt = dumpPath(txt, i_path)
        txt
      end
    end
  end
  return out_txt
end

function dumpStringCommentOption(in_txt::Tpl.Text, in_a_cmt::Option{<:String})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_str::String
    @match (in_txt, in_a_cmt) begin
      (txt, SOME(i_str)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(" "))
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("\\"))
        @assign txt = Tpl.writeStr(txt, i_str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("\\"))
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function lm_139(in_txt::Tpl.Text, in_items::List{<:Absyn.TypeSpec})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Absyn.TypeSpec}
    local i_ty::Absyn.TypeSpec
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_ty <| rest) => begin
        @assign txt = dumpTypeSpec(txt, i_ty)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_139(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function dumpTypeSpec(in_txt::Tpl.Text, in_a_typeSpec::Absyn.TypeSpec)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_typeSpecs::List{Absyn.TypeSpec}
    local i_arrayDim::Option{Absyn.ArrayDim}
    local i_path::Absyn.Path
    local l_ty__str::Tpl.Text
    local l_arraydim__str::Tpl.Text
    local l_path__str::Tpl.Text
    @match (in_txt, in_a_typeSpec) begin
      (txt, Absyn.TPATH(path = i_path, arrayDim = i_arrayDim)) => begin
        @assign l_path__str = dumpPath(Tpl.emptyTxt, i_path)
        @assign l_arraydim__str = dumpArrayDimOpt(Tpl.emptyTxt, i_arrayDim)
        @assign txt = Tpl.writeText(txt, l_path__str)
        @assign txt = Tpl.writeText(txt, l_arraydim__str)
        txt
      end

      (
        txt,
        Absyn.TCOMPLEX(path = i_path, typeSpecs = i_typeSpecs, arrayDim = i_arrayDim),
      ) => begin
        @assign l_path__str = dumpPath(Tpl.emptyTxt, i_path)
        @assign l_ty__str = Tpl.pushIter(
          Tpl.emptyTxt,
          Tpl.ITER_OPTIONS(
            0,
            NONE(),
            SOME(Tpl.ST_STRING(", ")),
            0,
            0,
            Tpl.ST_NEW_LINE(),
            0,
            Tpl.ST_NEW_LINE(),
          ),
        )
        @assign l_ty__str = lm_139(l_ty__str, i_typeSpecs)
        @assign l_ty__str = Tpl.popIter(l_ty__str)
        @assign l_arraydim__str = dumpArrayDimOpt(Tpl.emptyTxt, i_arrayDim)
        @assign txt = Tpl.writeText(txt, l_path__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("<"))
        @assign txt = Tpl.writeText(txt, l_ty__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(">"))
        @assign txt = Tpl.writeText(txt, l_arraydim__str)
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function dumpArrayDimOpt(
  in_txt::Tpl.Text,
  in_a_arraydim::Option{<:Absyn.ArrayDim},
)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_ad::Absyn.ArrayDim
    @match (in_txt, in_a_arraydim) begin
      (txt, SOME(i_ad)) => begin
        @assign txt = dumpSubscripts(txt, i_ad)
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function lm_142(in_txt::Tpl.Text, in_items::List{<:Absyn.Subscript})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Absyn.Subscript}
    local i_s::Absyn.Subscript
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_s <| rest) => begin
        @assign txt = dumpSubscript(txt, i_s)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_142(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function dumpSubscripts(
  in_txt::Tpl.Text,
  in_a_subscripts::List{<:Absyn.Subscript},
)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_subscripts::List{Absyn.Subscript}
    local l_sub__str::Tpl.Text
    @match (in_txt, in_a_subscripts) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_subscripts) => begin
        @assign l_sub__str = Tpl.pushIter(
          Tpl.emptyTxt,
          Tpl.ITER_OPTIONS(
            0,
            NONE(),
            SOME(Tpl.ST_STRING(", ")),
            0,
            0,
            Tpl.ST_NEW_LINE(),
            0,
            Tpl.ST_NEW_LINE(),
          ),
        )
        @assign l_sub__str = lm_142(l_sub__str, i_subscripts)
        @assign l_sub__str = Tpl.popIter(l_sub__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("["))
        @assign txt = Tpl.writeText(txt, l_sub__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("]"))
        txt
      end
    end
  end
  return out_txt
end

function dumpSubscript(in_txt::Tpl.Text, in_a_subscript::Absyn.Subscript)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_subscript::Absyn.Exp
    @match (in_txt, in_a_subscript) begin
      (txt, Absyn.NOSUB(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(":"))
        txt
      end

      (txt, Absyn.SUBSCRIPT(subscript = i_subscript)) => begin
        @assign txt = dumpExp(txt, i_subscript)
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function lm_145(in_txt::Tpl.Text, in_items::List{<:Absyn.Exp})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Absyn.Exp}
    local i_e::Absyn.Exp
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_e <| rest) => begin
        @assign txt = dumpExp(txt, i_e)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_145(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function lm_146(in_txt::Tpl.Text, in_items::List{<:Absyn.Exp})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Absyn.Exp}
    local i_e::Absyn.Exp
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_e <| rest) => begin
        @assign txt = dumpExp(txt, i_e)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_146(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function lm_147(in_txt::Tpl.Text, in_items::List{<:List{<:Absyn.Exp}})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{List{Absyn.Exp}}
    local i_row::List{Absyn.Exp}
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_row <| rest) => begin
        @assign txt = Tpl.pushIter(
          txt,
          Tpl.ITER_OPTIONS(
            0,
            NONE(),
            SOME(Tpl.ST_STRING(", ")),
            0,
            0,
            Tpl.ST_NEW_LINE(),
            0,
            Tpl.ST_NEW_LINE(),
          ),
        )
        @assign txt = lm_146(txt, i_row)
        @assign txt = Tpl.popIter(txt)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_147(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function lm_148(in_txt::Tpl.Text, in_items::List{<:Absyn.Exp})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Absyn.Exp}
    local i_e::Absyn.Exp
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_e <| rest) => begin
        @assign txt = dumpExp(txt, i_e)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_148(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function lm_149(in_txt::Tpl.Text, in_items::List{<:Absyn.Exp})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Absyn.Exp}
    local i_e::Absyn.Exp
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_e <| rest) => begin
        @assign txt = dumpExp(txt, i_e)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_149(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function dumpExp(in_txt::Tpl.Text, in_a_exp::Absyn.Exp)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_index::Absyn.Exp
    local i_exps::List{Absyn.Exp}
    local i_rest::Absyn.Exp
    local i_head::Absyn.Exp
    local i_id::Absyn.Ident
    local i_code::Absyn.CodeNode
    local i_expressions::List{Absyn.Exp}
    local i_stop::Absyn.Exp
    local i_step::Absyn.Exp
    local i_start::Absyn.Exp
    local i_matrix::List{List{Absyn.Exp}}
    local i_arrayExp::List{Absyn.Exp}
    local i_function__::Absyn.ComponentRef
    local i_functionArgs::Absyn.FunctionArgs
    local i_exp::Absyn.Exp
    local i_op::Absyn.Operator
    local i_exp2::Absyn.Exp
    local i_e::Absyn.Exp
    local i_exp1::Absyn.Exp
    local i_value_2::Bool
    local i_componentRef::Absyn.ComponentRef
    local i_value_1::String
    local i_value::Integer
    local l_list__str::Tpl.Text
    local l_rest__str::Tpl.Text
    local l_head__str::Tpl.Text
    local l_tuple__str::Tpl.Text
    local l_stop__str::Tpl.Text
    local l_step__str::Tpl.Text
    local l_start__str::Tpl.Text
    local l_matrix__str::Tpl.Text
    local l_array__str::Tpl.Text
    local l_func__str::Tpl.Text
    local l_args__str::Tpl.Text
    local l_exp__str::Tpl.Text
    local l_op__str::Tpl.Text
    local l_rhs__str::Tpl.Text
    local l_lhs__str::Tpl.Text
    @match (in_txt, in_a_exp) begin
      (txt, Absyn.INTEGER(value = i_value)) => begin
        @assign txt = Tpl.writeStr(txt, intString(i_value))
        txt
      end

      (txt, Absyn.REAL(value = i_value_1)) => begin
        @assign txt = Tpl.writeStr(txt, i_value_1)
        txt
      end

      (txt, Absyn.CREF(componentRef = i_componentRef)) => begin
        @assign txt = dumpCref(txt, i_componentRef)
        txt
      end

      (txt, Absyn.STRING(value = i_value_1)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("\\"))
        @assign txt = Tpl.pushBlock(txt, Tpl.BT_ABS_INDENT(0))
        @assign txt = Tpl.writeStr(txt, i_value_1)
        @assign txt = Tpl.popBlock(txt)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("\\"))
        txt
      end

      (txt, Absyn.BOOL(value = i_value_2)) => begin
        @assign txt = Tpl.writeStr(txt, Tpl.booleanString(i_value_2))
        txt
      end

      (txt, i_e && Absyn.BINARY(exp1 = i_exp1, exp2 = i_exp2, op = i_op)) => begin
        @assign l_lhs__str = dumpOperand(Tpl.emptyTxt, i_exp1, i_e, true)
        @assign l_rhs__str = dumpOperand(Tpl.emptyTxt, i_exp2, i_e, false)
        @assign l_op__str = dumpOperator(Tpl.emptyTxt, i_op)
        @assign txt = Tpl.writeText(txt, l_lhs__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(" "))
        @assign txt = Tpl.writeText(txt, l_op__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(" "))
        @assign txt = Tpl.writeText(txt, l_rhs__str)
        txt
      end

      (txt, i_e && Absyn.UNARY(exp = i_exp, op = i_op)) => begin
        @assign l_exp__str = dumpOperand(Tpl.emptyTxt, i_exp, i_e, false)
        @assign l_op__str = dumpOperator(Tpl.emptyTxt, i_op)
        @assign txt = Tpl.writeText(txt, l_op__str)
        @assign txt = Tpl.writeText(txt, l_exp__str)
        txt
      end

      (txt, i_e && Absyn.LBINARY(exp1 = i_exp1, exp2 = i_exp2, op = i_op)) => begin
        @assign l_lhs__str = dumpOperand(Tpl.emptyTxt, i_exp1, i_e, true)
        @assign l_rhs__str = dumpOperand(Tpl.emptyTxt, i_exp2, i_e, false)
        @assign l_op__str = dumpOperator(Tpl.emptyTxt, i_op)
        @assign txt = Tpl.writeText(txt, l_lhs__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(" "))
        @assign txt = Tpl.writeText(txt, l_op__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(" "))
        @assign txt = Tpl.writeText(txt, l_rhs__str)
        txt
      end

      (txt, i_e && Absyn.LUNARY(exp = i_exp, op = i_op)) => begin
        @assign l_exp__str = dumpOperand(Tpl.emptyTxt, i_exp, i_e, false)
        @assign l_op__str = dumpOperator(Tpl.emptyTxt, i_op)
        @assign txt = Tpl.writeText(txt, l_op__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(" "))
        @assign txt = Tpl.writeText(txt, l_exp__str)
        txt
      end

      (txt, i_e && Absyn.RELATION(exp1 = i_exp1, exp2 = i_exp2, op = i_op)) => begin
        @assign l_lhs__str = dumpOperand(Tpl.emptyTxt, i_exp1, i_e, true)
        @assign l_rhs__str = dumpOperand(Tpl.emptyTxt, i_exp2, i_e, false)
        @assign l_op__str = dumpOperator(Tpl.emptyTxt, i_op)
        @assign txt = Tpl.writeText(txt, l_lhs__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(" "))
        @assign txt = Tpl.writeText(txt, l_op__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(" "))
        @assign txt = Tpl.writeText(txt, l_rhs__str)
        txt
      end

      (txt, i_exp && Absyn.IFEXP(ifExp = _)) => begin
        @assign txt = dumpIfExp(txt, i_exp)
        txt
      end

      (
        txt,
        Absyn.CALL(
          function_ = Absyn.CREF_IDENT(name = "$array"),
          functionArgs = i_functionArgs,
        ),
      ) => begin
        @assign l_args__str = dumpFunctionArgs(Tpl.emptyTxt, i_functionArgs)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("{"))
        @assign txt = Tpl.writeText(txt, l_args__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("}"))
        txt
      end

      (txt, Absyn.CALL(function_ = i_function__, functionArgs = i_functionArgs)) => begin
        @assign l_func__str = dumpCref(Tpl.emptyTxt, i_function__)
        @assign l_args__str = dumpFunctionArgs(Tpl.emptyTxt, i_functionArgs)
        @assign txt = Tpl.writeText(txt, l_func__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("("))
        @assign txt = Tpl.writeText(txt, l_args__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(")"))
        txt
      end

      (
        txt,
        Absyn.PARTEVALFUNCTION(function_ = i_function__, functionArgs = i_functionArgs),
      ) => begin
        @assign l_func__str = dumpCref(Tpl.emptyTxt, i_function__)
        @assign l_args__str = dumpFunctionArgs(Tpl.emptyTxt, i_functionArgs)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("function "))
        @assign txt = Tpl.writeText(txt, l_func__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("("))
        @assign txt = Tpl.writeText(txt, l_args__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(")"))
        txt
      end

      (txt, Absyn.ARRAY(arrayExp = i_arrayExp)) => begin
        @assign l_array__str = Tpl.pushIter(
          Tpl.emptyTxt,
          Tpl.ITER_OPTIONS(
            0,
            NONE(),
            SOME(Tpl.ST_STRING(", ")),
            0,
            0,
            Tpl.ST_NEW_LINE(),
            0,
            Tpl.ST_NEW_LINE(),
          ),
        )
        @assign l_array__str = lm_145(l_array__str, i_arrayExp)
        @assign l_array__str = Tpl.popIter(l_array__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("{"))
        @assign txt = Tpl.writeText(txt, l_array__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("}"))
        txt
      end

      (txt, Absyn.MATRIX(matrix = i_matrix)) => begin
        @assign l_matrix__str = Tpl.pushIter(
          Tpl.emptyTxt,
          Tpl.ITER_OPTIONS(
            0,
            NONE(),
            SOME(Tpl.ST_STRING("; ")),
            0,
            0,
            Tpl.ST_NEW_LINE(),
            0,
            Tpl.ST_NEW_LINE(),
          ),
        )
        @assign l_matrix__str = lm_147(l_matrix__str, i_matrix)
        @assign l_matrix__str = Tpl.popIter(l_matrix__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("["))
        @assign txt = Tpl.writeText(txt, l_matrix__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("]"))
        txt
      end

      (txt, i_e && Absyn.RANGE(step = SOME(i_step), start = i_start, stop = i_stop)) =>
        begin
          @assign l_start__str = dumpOperand(Tpl.emptyTxt, i_start, i_e, false)
          @assign l_step__str = dumpOperand(Tpl.emptyTxt, i_step, i_e, false)
          @assign l_stop__str = dumpOperand(Tpl.emptyTxt, i_stop, i_e, false)
          @assign txt = Tpl.writeText(txt, l_start__str)
          @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(":"))
          @assign txt = Tpl.writeText(txt, l_step__str)
          @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(":"))
          @assign txt = Tpl.writeText(txt, l_stop__str)
          txt
        end

      (txt, i_e && Absyn.RANGE(step = NONE(), start = i_start, stop = i_stop)) => begin
        @assign l_start__str = dumpOperand(Tpl.emptyTxt, i_start, i_e, false)
        @assign l_stop__str = dumpOperand(Tpl.emptyTxt, i_stop, i_e, false)
        @assign txt = Tpl.writeText(txt, l_start__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(":"))
        @assign txt = Tpl.writeText(txt, l_stop__str)
        txt
      end

      (txt, Absyn.TUPLE(expressions = i_expressions)) => begin
        @assign l_tuple__str = Tpl.pushIter(
          Tpl.emptyTxt,
          Tpl.ITER_OPTIONS(
            0,
            SOME(Tpl.ST_STRING("")),
            SOME(Tpl.ST_STRING(", ")),
            0,
            0,
            Tpl.ST_NEW_LINE(),
            0,
            Tpl.ST_NEW_LINE(),
          ),
        )
        @assign l_tuple__str = lm_148(l_tuple__str, i_expressions)
        @assign l_tuple__str = Tpl.popIter(l_tuple__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("("))
        @assign txt = Tpl.writeText(txt, l_tuple__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(")"))
        txt
      end

      (txt, Absyn.END(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("end"))
        txt
      end

      (txt, Absyn.CODE(code = i_code)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("Code("))
        @assign txt = dumpCodeNode(txt, i_code)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(")"))
        txt
      end

      (txt, Absyn.AS(exp = i_exp, id = i_id)) => begin
        @assign l_exp__str = dumpExp(Tpl.emptyTxt, i_exp)
        @assign txt = Tpl.writeStr(txt, i_id)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(" as "))
        @assign txt = Tpl.writeText(txt, l_exp__str)
        txt
      end

      (txt, Absyn.CONS(head = i_head, rest = i_rest)) => begin
        @assign l_head__str = dumpExp(Tpl.emptyTxt, i_head)
        @assign l_rest__str = dumpExp(Tpl.emptyTxt, i_rest)
        @assign txt = Tpl.writeText(txt, l_head__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(" :: "))
        @assign txt = Tpl.writeText(txt, l_rest__str)
        txt
      end

      (txt, i_exp && Absyn.MATCHEXP(matchTy = _)) => begin
        @assign txt = dumpMatchExp(txt, i_exp)
        txt
      end

      (txt, Absyn.LIST(exps = i_exps)) => begin
        @assign l_list__str = Tpl.pushIter(
          Tpl.emptyTxt,
          Tpl.ITER_OPTIONS(
            0,
            NONE(),
            SOME(Tpl.ST_STRING(", ")),
            0,
            0,
            Tpl.ST_NEW_LINE(),
            0,
            Tpl.ST_NEW_LINE(),
          ),
        )
        @assign l_list__str = lm_149(l_list__str, i_exps)
        @assign l_list__str = Tpl.popIter(l_list__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("{"))
        @assign txt = Tpl.writeText(txt, l_list__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("}"))
        txt
      end

      (txt, Absyn.DOT(exp = i_exp, index = i_index)) => begin
        @assign txt = dumpExp(txt, i_exp)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("."))
        @assign txt = dumpExp(txt, i_index)
        txt
      end

      (txt, _) => begin
        @assign txt = Tpl.writeTok(
          txt,
          Tpl.ST_STRING("/* AbsynDumpTpl.dumpExp: UNHANDLED Abyn.Exp */"),
        )
        txt
      end
    end
  end
  return out_txt
end

function dumpLhsExp(in_txt::Tpl.Text, in_a_lhs::Absyn.Exp)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_lhs::Absyn.Exp
    @match (in_txt, in_a_lhs) begin
      (txt, i_lhs && Absyn.IFEXP(ifExp = _)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("("))
        @assign txt = dumpExp(txt, i_lhs)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(")"))
        txt
      end

      (txt, i_lhs) => begin
        @assign txt = dumpExp(txt, i_lhs)
        txt
      end
    end
  end
  return out_txt
end

function fun_152(in_txt::Tpl.Text, in_mArg::Bool, in_a_op__str::Tpl.Text)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local a_op__str::Tpl.Text
    @match (in_txt, in_mArg, in_a_op__str) begin
      (txt, false, a_op__str) => begin
        @assign txt = Tpl.writeText(txt, a_op__str)
        txt
      end

      (txt, _, a_op__str) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("("))
        @assign txt = Tpl.writeText(txt, a_op__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(")"))
        txt
      end
    end
  end
  return out_txt
end

function dumpOperand(
  txt::Tpl.Text,
  a_operand::Absyn.Exp,
  a_operation::Absyn.Exp,
  a_lhs::Bool,
)::Tpl.Text
  local out_txt::Tpl.Text

  local ret_1::Bool
  local l_op__str::Tpl.Text

  @assign l_op__str = dumpExp(Tpl.emptyTxt, a_operand)
  @assign ret_1 = Dump.shouldParenthesize(a_operand, a_operation, a_lhs)
  @assign out_txt = fun_152(txt, ret_1, l_op__str)
  return out_txt
end

function dumpIfExp(in_txt::Tpl.Text, in_a_if__exp::Absyn.Exp)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_elseIfBranch::List{Tuple{Absyn.Exp, Absyn.Exp}}
    local i_elseBranch::Absyn.Exp
    local i_trueBranch::Absyn.Exp
    local i_ifExp::Absyn.Exp
    local l_else__if__str::Tpl.Text
    local l_else__branch__str::Tpl.Text
    local l_true__branch__str::Tpl.Text
    local l_cond__str::Tpl.Text
    @match (in_txt, in_a_if__exp) begin
      (
        txt,
        Absyn.IFEXP(
          ifExp = i_ifExp,
          trueBranch = i_trueBranch,
          elseBranch = i_elseBranch,
          elseIfBranch = i_elseIfBranch,
        ),
      ) => begin
        @assign l_cond__str = dumpExp(Tpl.emptyTxt, i_ifExp)
        @assign l_true__branch__str = dumpExp(Tpl.emptyTxt, i_trueBranch)
        @assign l_else__branch__str = dumpExp(Tpl.emptyTxt, i_elseBranch)
        @assign l_else__if__str = dumpElseIfExp(Tpl.emptyTxt, i_elseIfBranch)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("if "))
        @assign txt = Tpl.writeText(txt, l_cond__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(" then "))
        @assign txt = Tpl.writeText(txt, l_true__branch__str)
        @assign txt = Tpl.writeText(txt, l_else__if__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(" else "))
        @assign txt = Tpl.writeText(txt, l_else__branch__str)
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function lm_155(in_txt::Tpl.Text, in_items::List{<:Tuple{<:Absyn.Exp, Absyn.Exp}})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Tuple{Absyn.Exp, Absyn.Exp}}
    local i_branch::Absyn.Exp
    local i_cond::Absyn.Exp
    local l_branch__str::Tpl.Text
    local l_cond__str::Tpl.Text
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, (i_cond, i_branch) <| rest) => begin
        @assign l_cond__str = dumpExp(Tpl.emptyTxt, i_cond)
        @assign l_branch__str = dumpExp(Tpl.emptyTxt, i_branch)
        @assign txt = Tpl.pushBlock(txt, Tpl.BT_INDENT(1))
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("elseif "))
        @assign txt = Tpl.writeText(txt, l_cond__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(" then "))
        @assign txt = Tpl.writeText(txt, l_branch__str)
        @assign txt = Tpl.popBlock(txt)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_155(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function dumpElseIfExp(
  txt::Tpl.Text,
  a_else__if::List{<:Tuple{<:Absyn.Exp, Absyn.Exp}},
)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = Tpl.pushIter(
    txt,
    Tpl.ITER_OPTIONS(
      0,
      NONE(),
      SOME(Tpl.ST_NEW_LINE()),
      0,
      0,
      Tpl.ST_NEW_LINE(),
      0,
      Tpl.ST_NEW_LINE(),
    ),
  )
  @assign out_txt = lm_155(out_txt, a_else__if)
  @assign out_txt = Tpl.popIter(out_txt)
  return out_txt
end

function fun_157(in_txt::Tpl.Text, in_a_boolean::Bool)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    @match (in_txt, in_a_boolean) begin
      (txt, false) => begin
        txt
      end

      (txt, _) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("initial "))
        txt
      end
    end
  end
  return out_txt
end

function fun_158(in_txt::Tpl.Text, in_a_boolean::Bool)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    @match (in_txt, in_a_boolean) begin
      (txt, false) => begin
        txt
      end

      (txt, _) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("initial "))
        txt
      end
    end
  end
  return out_txt
end

function fun_159(in_txt::Tpl.Text, in_a_boolean::Bool)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    @match (in_txt, in_a_boolean) begin
      (txt, false) => begin
        txt
      end

      (txt, _) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("initial "))
        txt
      end
    end
  end
  return out_txt
end

function dumpCodeNode(in_txt::Tpl.Text, in_a_code::Absyn.CodeNode)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_modification::Absyn.Modification
    local i_exp::Absyn.Exp
    local i_element::Absyn.Element
    local i_algorithmItemLst::List{Absyn.AlgorithmItem}
    local i_equationItemLst::List{Absyn.EquationItem}
    local i_boolean::Bool
    local i_componentRef::Absyn.ComponentRef
    local i_path::Absyn.Path
    local l_algs__str::Tpl.Text
    local l_eql__str::Tpl.Text
    local l_initial__str::Tpl.Text
    @match (in_txt, in_a_code) begin
      (txt, Absyn.C_TYPENAME(path = i_path)) => begin
        @assign txt = dumpPath(txt, i_path)
        txt
      end

      (txt, Absyn.C_VARIABLENAME(componentRef = i_componentRef)) => begin
        @assign txt = dumpCref(txt, i_componentRef)
        txt
      end

      (
        txt,
        Absyn.C_CONSTRAINTSECTION(boolean = i_boolean, equationItemLst = i_equationItemLst),
      ) => begin
        @assign l_initial__str = fun_157(Tpl.emptyTxt, i_boolean)
        @assign l_eql__str = dumpEquationItems(Tpl.emptyTxt, i_equationItemLst)
        @assign txt = Tpl.writeText(txt, l_initial__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_LINE("constraint\\n"))
        @assign txt = Tpl.pushBlock(txt, Tpl.BT_INDENT(2))
        @assign txt = Tpl.writeText(txt, l_eql__str)
        @assign txt = Tpl.popBlock(txt)
        txt
      end

      (
        txt,
        Absyn.C_EQUATIONSECTION(boolean = i_boolean, equationItemLst = i_equationItemLst),
      ) => begin
        @assign l_initial__str = fun_158(Tpl.emptyTxt, i_boolean)
        @assign l_eql__str = dumpEquationItems(Tpl.emptyTxt, i_equationItemLst)
        @assign txt = Tpl.writeText(txt, l_initial__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_LINE("equation\\n"))
        @assign txt = Tpl.pushBlock(txt, Tpl.BT_INDENT(2))
        @assign txt = Tpl.writeText(txt, l_eql__str)
        @assign txt = Tpl.popBlock(txt)
        txt
      end

      (
        txt,
        Absyn.C_ALGORITHMSECTION(
          boolean = i_boolean,
          algorithmItemLst = i_algorithmItemLst,
        ),
      ) => begin
        @assign l_initial__str = fun_159(Tpl.emptyTxt, i_boolean)
        @assign l_algs__str = dumpAlgorithmItems(Tpl.emptyTxt, i_algorithmItemLst)
        @assign txt = Tpl.writeText(txt, l_initial__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_LINE("algorithm\\n"))
        @assign txt = Tpl.pushBlock(txt, Tpl.BT_INDENT(2))
        @assign txt = Tpl.writeText(txt, l_algs__str)
        @assign txt = Tpl.popBlock(txt)
        txt
      end

      (txt, Absyn.C_ELEMENT(element = i_element)) => begin
        @assign txt = dumpElement(txt, i_element, Dump.defaultDumpOptions)
        txt
      end

      (txt, Absyn.C_EXPRESSION(exp = i_exp)) => begin
        @assign txt = dumpExp(txt, i_exp)
        txt
      end

      (txt, Absyn.C_MODIFICATION(modification = i_modification)) => begin
        @assign txt = dumpModification(txt, i_modification)
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function lm_161(in_txt::Tpl.Text, in_items::List{<:Absyn.Case})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Absyn.Case}
    local i_c::Absyn.Case
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_c <| rest) => begin
        @assign txt = dumpMatchCase(txt, i_c)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_161(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function dumpMatchExp(in_txt::Tpl.Text, in_a_match__exp::Absyn.Exp)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_comment::Option{String}
    local i_cases::List{Absyn.Case}
    local i_localDecls::List{Absyn.ElementItem}
    local i_inputExp::Absyn.Exp
    local i_matchTy::Absyn.MatchType
    local l_cmt__str::Tpl.Text
    local l_cases__str::Tpl.Text
    local l_locals__str::Tpl.Text
    local l_input__str::Tpl.Text
    local l_ty__str::Tpl.Text
    @match (in_txt, in_a_match__exp) begin
      (
        txt,
        Absyn.MATCHEXP(
          matchTy = i_matchTy,
          inputExp = i_inputExp,
          localDecls = i_localDecls,
          cases = i_cases,
          comment = i_comment,
        ),
      ) => begin
        @assign l_ty__str = dumpMatchType(Tpl.emptyTxt, i_matchTy)
        @assign l_input__str = dumpExp(Tpl.emptyTxt, i_inputExp)
        @assign l_locals__str = dumpMatchLocals(Tpl.emptyTxt, i_localDecls)
        @assign l_cases__str = Tpl.pushIter(
          Tpl.emptyTxt,
          Tpl.ITER_OPTIONS(
            0,
            NONE(),
            SOME(Tpl.ST_STRING_LIST(list("\\n", "\\n"), true)),
            0,
            0,
            Tpl.ST_NEW_LINE(),
            0,
            Tpl.ST_NEW_LINE(),
          ),
        )
        @assign l_cases__str = lm_161(l_cases__str, i_cases)
        @assign l_cases__str = Tpl.popIter(l_cases__str)
        @assign l_cmt__str = dumpStringCommentOption(Tpl.emptyTxt, i_comment)
        @assign txt = Tpl.writeText(txt, l_ty__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(" "))
        @assign txt = Tpl.writeText(txt, l_input__str)
        @assign txt = Tpl.softNewLine(txt)
        @assign txt = Tpl.writeText(txt, l_locals__str)
        @assign txt = Tpl.softNewLine(txt)
        @assign txt = Tpl.pushBlock(txt, Tpl.BT_INDENT(2))
        @assign txt = Tpl.writeText(txt, l_cases__str)
        @assign txt = Tpl.writeText(txt, l_cmt__str)
        @assign txt = Tpl.softNewLine(txt)
        @assign txt = Tpl.popBlock(txt)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("end "))
        @assign txt = Tpl.writeText(txt, l_ty__str)
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function dumpMatchType(in_txt::Tpl.Text, in_a_match__type::Absyn.MatchType)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    @match (in_txt, in_a_match__type) begin
      (txt, Absyn.MATCH(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("match"))
        txt
      end

      (txt, Absyn.MATCHCONTINUE(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("matchcontinue"))
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function lm_164(in_txt::Tpl.Text, in_items::List{<:Absyn.ElementItem})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Absyn.ElementItem}
    local i_decl::Absyn.ElementItem
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_decl <| rest) => begin
        @assign txt = dumpElementItem(txt, i_decl, Dump.defaultDumpOptions)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_164(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function dumpMatchLocals(in_txt::Tpl.Text, in_a_locals::List{<:Absyn.ElementItem})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_locals::List{Absyn.ElementItem}
    @match (in_txt, in_a_locals) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_locals) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_LINE("  local\\n"))
        @assign txt = Tpl.pushBlock(txt, Tpl.BT_INDENT(4))
        @assign txt = Tpl.pushIter(
          txt,
          Tpl.ITER_OPTIONS(
            0,
            NONE(),
            SOME(Tpl.ST_NEW_LINE()),
            0,
            0,
            Tpl.ST_NEW_LINE(),
            0,
            Tpl.ST_NEW_LINE(),
          ),
        )
        @assign txt = lm_164(txt, i_locals)
        @assign txt = Tpl.popIter(txt)
        @assign txt = Tpl.softNewLine(txt)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_NEW_LINE())
        @assign txt = Tpl.popBlock(txt)
        txt
      end
    end
  end
  return out_txt
end

function lm_166(in_txt::Tpl.Text, in_items::List{<:Absyn.EquationItem})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Absyn.EquationItem}
    local i_eq::Absyn.EquationItem
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_eq <| rest) => begin
        @assign txt = dumpEquationItem(txt, i_eq)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_166(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function lm_167(in_txt::Tpl.Text, in_items::List{<:Absyn.AlgorithmItem})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Absyn.AlgorithmItem}
    local i_alg::Absyn.AlgorithmItem
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_alg <| rest) => begin
        @assign txt = dumpAlgorithmItem(txt, i_alg)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_167(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function dumpMatchEquations(in_txt::Tpl.Text, in_a_cp::Absyn.ClassPart)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_algs::List{Absyn.AlgorithmItem}
    local i_eql::List{Absyn.EquationItem}
    @match (in_txt, in_a_cp) begin
      (txt, Absyn.EQUATIONS(contents = nil())) => begin
        txt
      end

      (txt, Absyn.EQUATIONS(contents = i_eql)) => begin
        @assign txt =
          Tpl.writeTok(txt, Tpl.ST_STRING_LIST(list("\\n", "  equation\\n"), true))
        @assign txt = Tpl.pushBlock(txt, Tpl.BT_INDENT(4))
        @assign txt = Tpl.pushIter(
          txt,
          Tpl.ITER_OPTIONS(
            0,
            NONE(),
            SOME(Tpl.ST_NEW_LINE()),
            0,
            0,
            Tpl.ST_NEW_LINE(),
            0,
            Tpl.ST_NEW_LINE(),
          ),
        )
        @assign txt = lm_166(txt, i_eql)
        @assign txt = Tpl.popIter(txt)
        @assign txt = Tpl.popBlock(txt)
        txt
      end

      (txt, Absyn.ALGORITHMS(contents = nil())) => begin
        txt
      end

      (txt, Absyn.ALGORITHMS(contents = i_algs)) => begin
        @assign txt =
          Tpl.writeTok(txt, Tpl.ST_STRING_LIST(list("\\n", "  algorithm\\n"), true))
        @assign txt = Tpl.pushBlock(txt, Tpl.BT_INDENT(4))
        @assign txt = Tpl.pushIter(
          txt,
          Tpl.ITER_OPTIONS(
            0,
            NONE(),
            SOME(Tpl.ST_NEW_LINE()),
            0,
            0,
            Tpl.ST_NEW_LINE(),
            0,
            Tpl.ST_NEW_LINE(),
          ),
        )
        @assign txt = lm_167(txt, i_algs)
        @assign txt = Tpl.popIter(txt)
        @assign txt = Tpl.popBlock(txt)
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function fun_169(in_txt::Tpl.Text, in_a_patternGuard::Option{<:Absyn.Exp})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_g::Absyn.Exp
    @match (in_txt, in_a_patternGuard) begin
      (txt, SOME(i_g)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("guard "))
        @assign txt = dumpExp(txt, i_g)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(" "))
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function fun_170(
  in_txt::Tpl.Text,
  in_a_eql__str::Tpl.Text,
  in_a_result__str::Tpl.Text,
)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local a_result__str::Tpl.Text
    @match (in_txt, in_a_eql__str, in_a_result__str) begin
      (txt, Tpl.MEM_TEXT(tokens = nil()), a_result__str) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("then "))
        @assign txt = Tpl.writeText(txt, a_result__str)
        txt
      end

      (txt, _, a_result__str) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING_LIST(list("\\n", "  then\\n"), true))
        @assign txt = Tpl.pushBlock(txt, Tpl.BT_INDENT(4))
        @assign txt = Tpl.writeText(txt, a_result__str)
        @assign txt = Tpl.popBlock(txt)
        txt
      end
    end
  end
  return out_txt
end

function fun_171(
  in_txt::Tpl.Text,
  in_a_eql__str::Tpl.Text,
  in_a_result__str::Tpl.Text,
)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local a_result__str::Tpl.Text
    @match (in_txt, in_a_eql__str, in_a_result__str) begin
      (txt, Tpl.MEM_TEXT(tokens = nil()), a_result__str) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("then "))
        @assign txt = Tpl.writeText(txt, a_result__str)
        txt
      end

      (txt, _, a_result__str) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING_LIST(list("\\n", "  then\\n"), true))
        @assign txt = Tpl.pushBlock(txt, Tpl.BT_INDENT(4))
        @assign txt = Tpl.writeText(txt, a_result__str)
        @assign txt = Tpl.popBlock(txt)
        txt
      end
    end
  end
  return out_txt
end

function dumpMatchCase(in_txt::Tpl.Text, in_a_c::Absyn.Case)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_comment::Option{String}
    local i_result::Absyn.Exp
    local i_classPart::Absyn.ClassPart
    local i_patternGuard::Option{Absyn.Exp}
    local i_pattern::Absyn.Exp
    local l_cmt__str::Tpl.Text
    local l_then__str::Tpl.Text
    local l_result__str::Tpl.Text
    local l_eql__str::Tpl.Text
    local l_guard__str::Tpl.Text
    local l_pattern__str::Tpl.Text
    @match (in_txt, in_a_c) begin
      (
        txt,
        Absyn.CASE(
          pattern = i_pattern,
          patternGuard = i_patternGuard,
          classPart = i_classPart,
          result = i_result,
          comment = i_comment,
        ),
      ) => begin
        @assign l_pattern__str = dumpExp(Tpl.emptyTxt, i_pattern)
        @assign l_guard__str = fun_169(Tpl.emptyTxt, i_patternGuard)
        @assign l_eql__str = dumpMatchEquations(Tpl.emptyTxt, i_classPart)
        @assign l_result__str = dumpExp(Tpl.emptyTxt, i_result)
        @assign l_then__str = fun_170(Tpl.emptyTxt, l_eql__str, l_result__str)
        @assign l_cmt__str = dumpStringCommentOption(Tpl.emptyTxt, i_comment)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("case "))
        @assign txt = Tpl.writeText(txt, l_pattern__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(" "))
        @assign txt = Tpl.writeText(txt, l_guard__str)
        @assign txt = Tpl.writeText(txt, l_cmt__str)
        @assign txt = Tpl.writeText(txt, l_eql__str)
        @assign txt = Tpl.writeText(txt, l_then__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(";"))
        txt
      end

      (txt, Absyn.ELSE(classPart = i_classPart, result = i_result, comment = i_comment)) => begin
        @assign l_eql__str = dumpMatchEquations(Tpl.emptyTxt, i_classPart)
        @assign l_result__str = dumpExp(Tpl.emptyTxt, i_result)
        @assign l_then__str = fun_171(Tpl.emptyTxt, l_eql__str, l_result__str)
        @assign l_cmt__str = dumpStringCommentOption(Tpl.emptyTxt, i_comment)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("else "))
        @assign txt = Tpl.writeText(txt, l_cmt__str)
        @assign txt = Tpl.writeText(txt, l_eql__str)
        @assign txt = Tpl.writeText(txt, l_then__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(";"))
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function dumpOperator(in_txt::Tpl.Text, in_a_op::Absyn.Operator)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    @match (in_txt, in_a_op) begin
      (txt, Absyn.ADD(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("+"))
        txt
      end

      (txt, Absyn.SUB(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("-"))
        txt
      end

      (txt, Absyn.MUL(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("*"))
        txt
      end

      (txt, Absyn.DIV(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("/"))
        txt
      end

      (txt, Absyn.POW(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("^"))
        txt
      end

      (txt, Absyn.UPLUS(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("+"))
        txt
      end

      (txt, Absyn.UMINUS(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("-"))
        txt
      end

      (txt, Absyn.ADD_EW(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(".+"))
        txt
      end

      (txt, Absyn.SUB_EW(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(".-"))
        txt
      end

      (txt, Absyn.MUL_EW(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(".*"))
        txt
      end

      (txt, Absyn.DIV_EW(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("./"))
        txt
      end

      (txt, Absyn.POW_EW(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(".^"))
        txt
      end

      (txt, Absyn.UPLUS_EW(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(".+"))
        txt
      end

      (txt, Absyn.UMINUS_EW(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(".-"))
        txt
      end

      (txt, Absyn.AND(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("and"))
        txt
      end

      (txt, Absyn.OR(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("or"))
        txt
      end

      (txt, Absyn.NOT(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("not"))
        txt
      end

      (txt, Absyn.LESS(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("<"))
        txt
      end

      (txt, Absyn.LESSEQ(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("<="))
        txt
      end

      (txt, Absyn.GREATER(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(">"))
        txt
      end

      (txt, Absyn.GREATEREQ(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(">="))
        txt
      end

      (txt, Absyn.EQUAL(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("=="))
        txt
      end

      (txt, Absyn.NEQUAL(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("<>"))
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function fun_174(in_txt::Tpl.Text, in_mArg::Bool)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    @match (in_txt, in_mArg) begin
      (txt, false) => begin
        txt
      end

      (txt, _) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("_"))
        txt
      end
    end
  end
  return out_txt
end

function dumpCref(in_txt::Tpl.Text, in_a_cref::Absyn.ComponentRef)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_componentRef::Absyn.ComponentRef
    local i_subscripts::List{Absyn.Subscript}
    local i_name::Absyn.Ident
    local ret_0::Bool
    @match (in_txt, in_a_cref) begin
      (
        txt,
        Absyn.CREF_QUAL(
          name = i_name,
          subscripts = i_subscripts,
          componentRef = i_componentRef,
        ),
      ) => begin
        @assign txt = Tpl.writeStr(txt, i_name)
        @assign txt = dumpSubscripts(txt, i_subscripts)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("."))
        @assign txt = dumpCref(txt, i_componentRef)
        txt
      end

      (txt, Absyn.CREF_IDENT(name = i_name, subscripts = i_subscripts)) => begin
        @assign txt = Tpl.writeStr(txt, i_name)
        @assign txt = dumpSubscripts(txt, i_subscripts)
        txt
      end

      (txt, Absyn.CREF_FULLYQUALIFIED(componentRef = i_componentRef)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("."))
        @assign txt = dumpCref(txt, i_componentRef)
        txt
      end

      (txt, Absyn.WILD(__)) => begin
        @assign ret_0 = Config.acceptMetaModelicaGrammar()
        @assign txt = fun_174(txt, ret_0)
        txt
      end

      (txt, Absyn.ALLWILD(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("__"))
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function lm_176(in_txt::Tpl.Text, in_items::List{<:Absyn.Exp})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Absyn.Exp}
    local i_arg::Absyn.Exp
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_arg <| rest) => begin
        @assign txt = dumpExp(txt, i_arg)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_176(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function lm_177(in_txt::Tpl.Text, in_items::List{<:Absyn.NamedArg})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::List{Absyn.NamedArg}
    local i_narg::Absyn.NamedArg
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_narg <| rest) => begin
        @assign txt = dumpNamedArg(txt, i_narg)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_177(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function fun_178(in_txt::Tpl.Text, in_a_argNames::List{<:Absyn.NamedArg})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    @match (in_txt, in_a_argNames) begin
      (txt, nil()) => begin
        txt
      end

      (txt, _) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(", "))
        txt
      end
    end
  end
  return out_txt
end

function fun_179(
  in_txt::Tpl.Text,
  in_a_args__str::Tpl.Text,
  in_a_argNames::List{<:Absyn.NamedArg},
)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local a_argNames::List{Absyn.NamedArg}
    @match (in_txt, in_a_args__str, in_a_argNames) begin
      (txt, Tpl.MEM_TEXT(tokens = nil()), _) => begin
        txt
      end

      (txt, _, a_argNames) => begin
        @assign txt = fun_178(txt, a_argNames)
        txt
      end
    end
  end
  return out_txt
end

function lm_180(in_txt::Tpl.Text, in_items::Absyn.ForIterators)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::Absyn.ForIterators
    local i_i::Absyn.ForIterator
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_i <| rest) => begin
        @assign txt = dumpForIterator(txt, i_i)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_180(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function fun_181(in_txt::Tpl.Text, in_a_iterType::Absyn.ReductionIterType)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    @match (in_txt, in_a_iterType) begin
      (txt, Absyn.THREAD(__)) => begin
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("threaded "))
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function dumpFunctionArgs(in_txt::Tpl.Text, in_a_args::Absyn.FunctionArgs)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_iterType::Absyn.ReductionIterType
    local i_iterators::Absyn.ForIterators
    local i_exp::Absyn.Exp
    local i_argNames::List{Absyn.NamedArg}
    local i_args::List{Absyn.Exp}
    local l_iter__str::Tpl.Text
    local l_exp__str::Tpl.Text
    local l_separator::Tpl.Text
    local l_namedargs__str::Tpl.Text
    local l_args__str::Tpl.Text
    @match (in_txt, in_a_args) begin
      (txt, Absyn.FUNCTIONARGS(args = i_args, argNames = i_argNames)) => begin
        @assign l_args__str = Tpl.pushIter(
          Tpl.emptyTxt,
          Tpl.ITER_OPTIONS(
            0,
            NONE(),
            SOME(Tpl.ST_STRING(", ")),
            0,
            0,
            Tpl.ST_NEW_LINE(),
            0,
            Tpl.ST_NEW_LINE(),
          ),
        )
        @assign l_args__str = lm_176(l_args__str, i_args)
        @assign l_args__str = Tpl.popIter(l_args__str)
        @assign l_namedargs__str = Tpl.pushIter(
          Tpl.emptyTxt,
          Tpl.ITER_OPTIONS(
            0,
            NONE(),
            SOME(Tpl.ST_STRING(", ")),
            0,
            0,
            Tpl.ST_NEW_LINE(),
            0,
            Tpl.ST_NEW_LINE(),
          ),
        )
        @assign l_namedargs__str = lm_177(l_namedargs__str, i_argNames)
        @assign l_namedargs__str = Tpl.popIter(l_namedargs__str)
        @assign l_separator = fun_179(Tpl.emptyTxt, l_args__str, i_argNames)
        @assign txt = Tpl.writeText(txt, l_args__str)
        @assign txt = Tpl.writeText(txt, l_separator)
        @assign txt = Tpl.writeText(txt, l_namedargs__str)
        txt
      end

      (
        txt,
        Absyn.FOR_ITER_FARG(exp = i_exp, iterators = i_iterators, iterType = i_iterType),
      ) => begin
        @assign l_exp__str = dumpExp(Tpl.emptyTxt, i_exp)
        @assign l_iter__str = Tpl.pushIter(
          Tpl.emptyTxt,
          Tpl.ITER_OPTIONS(
            0,
            NONE(),
            SOME(Tpl.ST_STRING(", ")),
            0,
            0,
            Tpl.ST_NEW_LINE(),
            0,
            Tpl.ST_NEW_LINE(),
          ),
        )
        @assign l_iter__str = lm_180(l_iter__str, i_iterators)
        @assign l_iter__str = Tpl.popIter(l_iter__str)
        @assign txt = Tpl.writeText(txt, l_exp__str)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(" "))
        @assign txt = fun_181(txt, i_iterType)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("for "))
        @assign txt = Tpl.writeText(txt, l_iter__str)
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function dumpNamedArg(in_txt::Tpl.Text, in_a_narg::Absyn.NamedArg)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_argValue::Absyn.Exp
    local i_argName::Absyn.Ident
    @match (in_txt, in_a_narg) begin
      (txt, Absyn.NAMEDARG(argName = i_argName, argValue = i_argValue)) => begin
        @assign txt = Tpl.writeStr(txt, i_argName)
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING(" = "))
        @assign txt = dumpExp(txt, i_argValue)
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function lm_184(in_txt::Tpl.Text, in_items::Absyn.ForIterators)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local rest::Absyn.ForIterators
    local i_i::Absyn.ForIterator
    @match (in_txt, in_items) begin
      (txt, nil()) => begin
        txt
      end

      (txt, i_i <| rest) => begin
        @assign txt = dumpForIterator(txt, i_i)
        @assign txt = Tpl.nextIter(txt)
        @assign txt = lm_184(txt, rest)
        txt
      end
    end
  end
  return out_txt
end

function dumpForIterators(txt::Tpl.Text, a_iters::Absyn.ForIterators)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = Tpl.pushIter(
    txt,
    Tpl.ITER_OPTIONS(
      0,
      NONE(),
      SOME(Tpl.ST_STRING(", ")),
      0,
      0,
      Tpl.ST_NEW_LINE(),
      0,
      Tpl.ST_NEW_LINE(),
    ),
  )
  @assign out_txt = lm_184(out_txt, a_iters)
  @assign out_txt = Tpl.popIter(out_txt)
  return out_txt
end

function fun_186(in_txt::Tpl.Text, in_a_range::Option{<:Absyn.Exp})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_r::Absyn.Exp
    @match (in_txt, in_a_range) begin
      (txt, SOME(i_r)) => begin
        @assign txt = Tpl.pushBlock(txt, Tpl.BT_INDENT(1))
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("in "))
        @assign txt = dumpExp(txt, i_r)
        @assign txt = Tpl.popBlock(txt)
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function fun_187(in_txt::Tpl.Text, in_a_guardExp::Option{<:Absyn.Exp})::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_g::Absyn.Exp
    @match (in_txt, in_a_guardExp) begin
      (txt, SOME(i_g)) => begin
        @assign txt = Tpl.pushBlock(txt, Tpl.BT_INDENT(1))
        @assign txt = Tpl.writeTok(txt, Tpl.ST_STRING("guard "))
        @assign txt = dumpExp(txt, i_g)
        @assign txt = Tpl.popBlock(txt)
        txt
      end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function dumpForIterator(in_txt::Tpl.Text, in_a_iterator::Absyn.ForIterator)::Tpl.Text
  local out_txt::Tpl.Text

  @assign out_txt = begin
    local txt::Tpl.Text
    local i_name::String
    local i_guardExp::Option{Absyn.Exp}
    local i_range::Option{Absyn.Exp}
    local l_guard__str::Tpl.Text
    local l_range__str::Tpl.Text
    @match (in_txt, in_a_iterator) begin
      (txt, Absyn.ITERATOR(range = i_range, guardExp = i_guardExp, name = i_name)) =>
        begin
          @assign l_range__str = fun_186(Tpl.emptyTxt, i_range)
          @assign l_guard__str = fun_187(Tpl.emptyTxt, i_guardExp)
          @assign txt = Tpl.writeStr(txt, i_name)
          @assign txt = Tpl.writeText(txt, l_guard__str)
          @assign txt = Tpl.writeText(txt, l_range__str)
          txt
        end

      (txt, _) => begin
        txt
      end
    end
  end
  return out_txt
end

function errorMsg(txt::Tpl.Text, a_errMessage::String)::Tpl.Text
  local out_txt::Tpl.Text

  Tpl.addTemplateError(a_errMessage)
  @assign out_txt = Tpl.writeStr(txt, a_errMessage)
  return out_txt
end

@exportAll()
end
