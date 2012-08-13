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

/// Expands and collapses type abbreviations used in F#.
module TypeAbbreviations =
    open System.Collections.Generic

    let private data =
        [
            "bool", "System.Boolean"
            "byte", "System.Byte"
            "sbyte", "System.SByte"
            "int16", "System.Int16"
            "uint16", "System.UInt16"
            "int", "System.Int32"
            "uint32", "System.UInt32"
            "int64", "System.Int64"
            "uint64", "System.UInt64"
            "nativeint", "System.IntPtr"
            "unativeint", "System.UIntPtr"
            "char", "System.Char"
            "string", "System.String"
            "decimal", "System.Decimal"
            "void", "System.Void"
            "single", "System.Float32"
            "double", "System.Double"
            "bigint", "System.Numerics.BigInteger"
            "obj", "System.Object"
            "exn", "System.Exception"
            "seq", "System.Collections.Generic.IEnumerable`1"
            "unit", "Microsoft.FSharp.Core.Unit"
            "ref", "Microsoft.FSharp.Core.FSharpRef`1"
            "option", "Microsoft.FSharp.Core.FSharpOption`1"
            "list", "Microsoft.FSharp.Collections.FSharpList`1"
            "Map", "Microsoft.FSharp.Collections.FSharpMap`2"
            "Set", "Microsoft.FSharp.Collections.FSharpSet`1"
            "Async", "Microsoft.FSharp.Control.FSharpAsync"
            "Async", "Microsoft.FSharp.Control.FSharpAsync`1"
            "AsyncBuilder", "Microsoft.FSharp.Control.FSharpAsyncBuilder"
            "AsyncReplyChannel", "Microsoft.FSharp.Control.FSharpAsyncReplyChannel`1"
            "DelegateEvent", "Microsoft.FSharp.Control.FSharpDelegateEvent`1"
            "Handler", "Microsoft.FSharp.Control.FSharpHandler`1"
            "Event", "Microsoft.FSharp.Control.FSharpEvent"
            "Event", "Microsoft.FSharp.Control.FSharpEvent`1"
            "Event", "Microsoft.FSharp.Control.FSharpEvent`2"
            "MailboxProcessor", "Microsoft.FSharp.Control.FSharpMailboxProcessor`1"
            "Choice", "Microsoft.FSharp.Core.FSharpChoice`2"
            "Choice", "Microsoft.FSharp.Core.FSharpChoice`3"
            "Choice", "Microsoft.FSharp.Core.FSharpChoice`4"
            "Choice", "Microsoft.FSharp.Core.FSharpChoice`5"
            "Choice", "Microsoft.FSharp.Core.FSharpChoice`6"
            "Choice", "Microsoft.FSharp.Core.FSharpChoice`7"
            "Expr", "Microsoft.FSharp.Quotations.FSharpExpr"
            "Expr", "Microsoft.FSharp.Quotations.FSharpExpr`1"
            "Var", "Microsoft.FSharp.Quotations.FSharpVar"
        ]

    let private encode =
        let d = Dictionary()
        for (a, n) in data do
            d.[n] <- a
        d

    let private decode =
        let d = Dictionary()
        for (a, n) in data do
            d.[a] <- n
        d

    /// Tries to abbreviate a type name defined by the namespace and
    /// the short name. For example, Abbreviate "System.Int32" = Some "int".
    let Abbreviate (fullName: string) =
        match encode.TryGetValue fullName with
        | true, v -> Some v
        | _ -> None

    /// Expands a given abbreviation to a namespace and a name.
    /// For example, Parse "int" = Some "System.Int32"
    let Parse (abbreviation: string) =
        match decode.TryGetValue abbreviation with
        | true, v -> Some v
        | _ -> None

