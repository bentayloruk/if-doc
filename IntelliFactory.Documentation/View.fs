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

/// Utilities for constructing views.
module View =
    open System
    open System.Text.RegularExpressions

    let TypeUrl (s : string) = 
        let tpMatch = Regex.Match(s, "<[^>]*>$")
        let s = 
            if tpMatch.Success
            then 
                let t = s.Substring(0, s.Length - tpMatch.Length)
                let typeParams = tpMatch.Value |> Seq.fold(fun acc c -> if c = ',' then acc + 1 else acc ) 1
                sprintf "%s-%d" t typeParams
            else s
        Uri.EscapeUriString s

    /// Escapes the given ID for HTML.
    let IdEncode (x: string) =
        let enc (c: char) =
            String.Format(":{0:x}", int c)
        "id:" + Regex.Replace(x, @"[^-_a-zA-Z0-9.]",
            new MatchEvaluator(fun m -> enc m.Value.[0]))

    /// Escapes the given HTML string.
    let HtmlEncode x =
        System.Web.HttpUtility.HtmlEncode x

