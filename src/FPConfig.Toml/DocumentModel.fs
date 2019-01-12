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

module DocumentModel =
    let mapStringValue = String >> SimpleValue
    
    let mapIntegerValue = Integer >> SimpleValue

    let mapFloatValue = Float >> SimpleValue

    let mapBoolValue =  Boolean >> SimpleValue

    let mapOffsetDateTimeValue = OffsetDateTime >> SimpleValue

    let mapLocalDateTimeValue = LocalDateTime >> SimpleValue

    let mapLocalDateValue = LocalDate >> SimpleValue

    let mapLocalTimeValue = LocalTime >> SimpleValue

    let mapArrayValue = Array