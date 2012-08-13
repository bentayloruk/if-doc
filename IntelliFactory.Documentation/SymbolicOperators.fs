(******************************************************************************

Copyright (C) 2011 IntelliFactory

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

******************************************************************************)

namespace IntelliFactory.Documentation

// performs reverse conversion for mangled name of symbolic operators
module SymbolicOperators = 
    
    open System

    let private nameParts = 
        [
            "Greater",  ">"
            "Less",     "<"
            "Plus",     "+"
            "Minus",    "-"
            "Multiply", "*"
            "Equals",   "="
            "Twiddle",  "~"
            "Percent",  "%"
            "Dot",      "."
            "Amp",      "&"
            "Bar",      "|"
            "At",       "@"
            "Hash",     "#"
            "Hat",      "^"
            "Bang",     "!"
            "Qmark",    "?"
            "Divide",   "/"
            "Dot",      "."
            "Colon",    ":"
            "LParen",   "("
            "Comma",    ","
            "RParen",   ")"
            "LBrack",   "["
            "RBrack",   "]"
        ] |> dict

    let private predefined = 
        [
            "op_Nil",                   "[]"
            "op_ColonColon",            "::"
            "op_Addition",              "+"
            "op_Subtraction",           "-"
            "op_Multiply",              "*"
            "op_Division",              "/"
            "op_Exponentiation",        "**"
            "op_Append",                "@"
            "op_Concatenate",           "^"
            "op_Modulus",               "%"
            "op_BitwiseAnd",            "&&&"
            "op_BitwiseOr",             "|||"
            "op_ExclusiveOr",           "^^^"
            "op_LeftShift",             "<<<"
            "op_LogicalNot",            "~~~"
            "op_RightShift",            ">>>"
            "op_UnaryPlus",             "~+"
            "op_UnaryNegation",         "~-"
            "op_Equality",              "="
            "op_Inequality",            "<>"
            "op_LessThanOrEqual",       "<="
            "op_GreaterThanOrEqual",    ">="
            "op_LessThan",              "<"
            "op_GreaterThan",           ">"
            "op_Dynamic",               "?"
            "op_DynamicAssignment",     "?<-"
            "op_PipeRight",             "|>"
            "op_PipeRight2",            "||>"
            "op_PipeRight3",            "|||>"
            "op_PipeLeft",              "<|"
            "op_PipeLeft2",             "<||"
            "op_PipeLeft3",             "<|||"
            "op_Dereference",           "!"
            "op_ComposeRight",          ">>"
            "op_ComposeLeft",           "<<"
            "op_Quotation",             "<@ @>"
            "op_QuotationUntyped",      "<@@ @@>"
            "op_Splice",                "~%"
            "op_SpliceUntyped",         "~%%"
            "op_AddressOf",             "~&"
            "op_IntegerAddressOf",      "~&&"
            "op_BooleanOr",             "||"
            "op_BooleanAnd",            "&&"
            "op_AdditionAssignment",    "+="
            "op_SubtractionAssignment", "-="
            "op_MultiplyAssignment",    "*="
            "op_DivisionAssignment",    "/="
            "op_Range",                 ".."
            "op_RangeStep",             ".. .."
        ] |> dict


    let private tryParse (name : string) = 
        let name = name.Substring 3
        let parts = 
            [
                let buf = Text.StringBuilder()
                for c in name do
                    if Char.IsUpper c && buf.Length > 0 
                    then 
                        yield buf.ToString()
                        buf.Length <- 0
                    buf.Append c |> ignore

                if buf.Length > 0 
                then yield buf.ToString()
            ]
        let valid = parts |> Seq.forall nameParts.ContainsKey
        if valid 
        then
            parts
                |> Seq.map (fun n -> nameParts.[n])
                |> String.concat ""
                |> Some
        else None

    let TryGet name = 
        match predefined.TryGetValue name with
        | true, v -> Some v
        | false, _ -> tryParse name