namespace FPConfig.Toml

open System

type SimpleValue =
    | String of string
    | Integer of int64
    | Float of float
    | Boolean of bool
    | OffsetDateTime of DateTimeOffset
    | LocalDateTime of DateTime
    | LocalDate of Date
    | LocalTime of Time

type Value =
    | InlineTableValue of Map<string,Value>
    | SimpleValue of SimpleValue
    | Array of Value list

type Table =
    | SimpleTable of Map<string,Value>
    | TableArray of Map<string,Value> list
