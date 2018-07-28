namespace FPConfig.Toml

open System
open System.Collections.ObjectModel

type PrimitiveValue =
    | String of string
    | Integer of int64
    | Float of float
    | Boolean of bool
    | OffsetDateTime of DateTimeOffset
    | LocalDateTime of DateTime
    | LocalDate of Date
    | LocalTime of Time

type ArrayValue = Array of PrimitiveValue

type Value =
    | InlineTableValue of ReadOnlyDictionary<string,Value>
    | PrimitiveValue of PrimitiveValue
    | ArrayValue of ArrayValue

/// This type is intentionally flat, rather than recursive.  While it can be reasonably said that TOML supports
/// nested tables, it can equally be said that nested tables can be represented in a flat structure with
/// dotted keys.  For example:
/// 
/// ```
/// [a]
/// key = value
///   [b]
///   key = value
/// ```
/// 
/// This structure could be represented as nested tables.  However, I think in most actual use cases it will be
/// simpler and safer to conceptually represent them like this:
/// 
/// ```
/// [a]
/// key = value
/// 
/// [a.b]
/// key = value
/// ```
/// 
/// This is not to mean that users should not nest tables - far from it.  This is simply to say that the Document Model
/// will represent them as flat structures, and leave nesting as a problem for parsing.
type Table =
    | SimpleTable of ReadOnlyDictionary<string,Value>
    | ArrayTable of ReadOnlyDictionary<string,Value> array

type Document = {
    Tables : ReadOnlyDictionary<string, Table>
}
