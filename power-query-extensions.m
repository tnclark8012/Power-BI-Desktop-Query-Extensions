let

MissingTests = List.Select(Record.FieldNames(_extensionLibrary), each not 
    List.MatchesAny(Tests, (testRecord as record) => Text.Contains(testRecord[CaseName], _))),
MissingDocumentation = List.Select(Record.FieldNames(_extensionLibrary), each not
    Record.HasFields(Value.Metadata(Value.Type(Record.Field(_extensionLibrary, _))), "Documentation.Examples")),
Tests = {
    [ 
        CaseName = "List.From", 
        Test = (library as record) => 
            TestUtils[AssertEqual](
                {"A", "B", "C"}, 
                library[List.From]("{A, B, C}"),
                "Text should be split on ,")
    ],
    [ 
        CaseName = "List.From - no braces", 
        Test = (library as record) => 
            TestUtils[AssertEqual](
                {"A", "B", "C"}, 
                library[List.From]("A, B, C"),
                "Text should be split on ,")
    ],
    [ 
        CaseName = "List.Flatten", 
        Test = (library as record) => 
            TestUtils[AssertEqual](
                { 1, 2, Table.FromRecords({[x=1]}), 3, 4, 5}, 
                library[List.Flatten]({ 1, 2, Table.FromRecords({[x=1]}), {3, 4, 5} }),
                "Flattened list")
    ],
    [ 
        CaseName = "Table.RenameColumn", 
        Test = (library as record) => 
            TestUtils[AssertEqual](
                {"NewName", "NumberCol"}, 
                Table.ColumnNames(library[Table.RenameColumn](TestUtils[SimpleTable], "TextCol", "NewName")),
                "Column should have new name")
    ],
    [
        CaseName = "Text.Until",
        Test = (library as record) => 
            TestUtils[AssertEqual](
                "abc ", 
                library[Text.Until]("abc 123", "1"),
                "Proper substring should be found")
    ],
    [
        CaseName = "Text.Substring - infer end",
        Test = (library as record) => 
            TestUtils[AssertEqual](
                "BI Rules!", 
                library[Text.Substring]("Power BI Rules!", 6),
                "Text.Substring('Power BI Rules', 6)")
    ],
    [
        CaseName = "Switch case pairs",
        Test = (library as record) => 
            TestUtils[AssertEqual](
                2, 
                library[Switch]("B", {{"A", 1}, {"B", 2}, {"C", 3}}),
                "B is second case")
    ],
    [
        CaseName = "Switch separate case and result",
        Test = (library as record) => 
            TestUtils[AssertEqual](
                4, 
                library[Switch]("TE", {"QB", "RB", "WR", "TE", "K", "D/ST"}, {1, 2, 3, 4, 5, 6}),
                "TE is 4th case")
    ]
},

TestUtils = [ 
    AssertEqual = (expected as any, actual as any, description as text) => 
        let
            expectedType = Value.Type(expected),
            actualType = Value.Type(actual),
            listAsText = (list as list) =>
                "{" & Text.Range(List.Accumulate(expected, "", (state, current) => state & ", " & Text.From(current)), 1) & " }",
            expectedAsText = if expected is list then listAsText(expected) else Text.From(expected),
            actualAsText = if expected is list then listAsText(actual) else Text.From(actual),
            typeAsText = (value as any) => 
                if value is binary then "binary" else
                if value is date then "date" else
                if value is datetime then "datetime" else
                if value is datetimezone then "datetimezone" else
                if value is duration then "duration" else
                if value is function then "function" else
                if value is list then "list" else
                if value is logical then "logical" else
                if value is none then "none" else
                if value is null then "null" else
                if value is number then "number" else
                if value is record then "record" else
                if value is table then "table" else
                if value is text then "text" else
                if value is time then "time" else
                if value is type then "type" else
                if value is any then "any"
                else "unknown -- not a primitive type!"
        in
            if not Value.Is(actualType, Value.Type(expectedType)) then 
                error "Expected type " & typeAsText(expectedType) & " does not match actual type " & typeAsText(actualType) 
//            else if expected is list then
//                List.Count(
            else if expected = actual then 
                true
            else if expectedAsText = actualAsText then error "Cannot provide accurate failure message! Text versions of expected and actual are identical, but they failed the equality test!"
            else error "Expected: " & expectedAsText & "; Actual: " & actualAsText & "; Reason: " & description,
    SimpleTable = Table.FromRecords({[TextCol = "A", NumberCol = "1"], [TextCol = "B", NumberCol = 2], [TextCol = "C", NumberCol = 3]})
],

TestResults = 
    let 
        failedTests = List.Select(
            List.Transform(Tests, (suite as record) => 
                let 
                    testResult = try suite[Test](_extensionLibrary)
                in
                    if testResult[HasError] then Error.Record(suite[CaseName], testResult[Error][Message], null) else true), 
            each _ <> true) 
    in 
        if List.IsEmpty(failedTests) then "All " & Text.From(List.Count(Tests)) & " tests passed! :)" else failedTests,

_extensionLibrary = [

Document = (name as text, description as text, valueOrExample as any, optional valueIfExample as any) =>
    let
        value = if valueIfExample is null then valueOrExample else valueIfExample,
        examples = if valueIfExample is null then {} else valueOrExample
    in
        Value.ReplaceType(value, Value.Type(value) meta [
            Documentation.Name = name, 
            Documentation.Description = description,  
            // [Description = "", Code="", Result =""]
            Documentation.Examples = examples
        ]),
///////////////////////// 
// Date                //
/////////////////////////
// Basic calendar
Date.Calendar = (optional start as any, optional end as any) => 
   let
      StartDate = Date.From(start),
      EndDate = Date.From(end),
         Source = Date.DatesBetween(StartDate, EndDate),
         FromList = Table.FromList(Source, Splitter.SplitByNothing(), null, null, ExtraValues.Error),
         Date = Table.RenameColumns(FromList,{{"Column1", "Date"}}),
         DayOfWeek = Table.AddColumn(Date, "Day of Week", each Date.DayName([Date])),
         Month = Table.AddColumn(DayOfWeek, "Month", each Date.MonthName([Date])),
         MonthNum = Table.AddColumn(Month, "MonthNumber", each Date.Month([Date])),
         WeekStartDate = Table.AddColumn(MonthNum, "WeekStartDate", each Date.StartOfWeek([Date])),
         WeekStart = Table.AddColumn(WeekStartDate, "Week Start", each [Month] & " " & Text.From(Date.Day([WeekStartDate]))),
         Year = Table.AddColumn(WeekStart, "Year", each Date.Year([Date])),
         YearMonth = Table.AddColumn(Year, "YearMonth", each Number.From(Text.From([Year]) & (if [MonthNumber] < 10 then "0" else "") & Text.From([MonthNumber]))),
         Result = YearMonth
   in
         Result,
// Dates between. Start and end can be flipped
Date.DatesBetween= (start as any, end as any) => 
      let 
        StartDate = Date.From(start), 
        EndDate = Date.From(end),
        adjustedStart = List.Min({StartDate, EndDate}),
        adjustedEnd = List.Max({StartDate, EndDate}),
        GetDates = (start as date, end as date, dates as list)=> if start > end then dates else @GetDates(Date.AddDays(start, 1), end, List.Combine({dates, {start}})),
        Dates = GetDates(adjustedStart, adjustedEnd, {})
      in Dates,

// Sunday is 0
Date.DayName = (date as any) => Switch(Date.DayOfWeek(DateTime.From(date)), {0, 1, 2, 3, 4, 5, 6}, {"Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"}, null),
Date.MonthName  = (date as any) => 
   let 
      monthNumber = if date is number then date else Date.Month(DateTime.From(date))
   in 
      Switch(
         monthNumber,
         {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12}, 
              {"January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"}, null),
   
/////////////////////////
// List                //
/////////////////////////
List.Flatten = Document(
    "List.Flatten",
    "Recursively flattens list elements. The end result is a single list",
    { [ Description = "Flattening nested lists into one", Code = "List.Flatten({ 1, 2, Table.FromRecords({[x=1]}), {3, 4, 5} })", Result = "{ 1, 2, Table.FromRecords({[x=1]}), 3, 4, 5}"] },
    (list as list) => List.Accumulate(list, {}, (state, current) =>
        let
            currentListContent = if current is list then @List.Flatten(current) else {current}
        in
            List.Combine({state, currentListContent})
    )
),
List.From = Document(
    "List.From", 
    "Converts a text representation of a list into a list of the elements. Items are considered to be split by ,",
    { [ Description = "Convert a text list", Code = "List.From(""{A, B, C}"")", Result = "{ ""A"", ""B"", ""C"" }"] },
    (simpleTextList as text) =>
        let
            trimWhitespace = Text.Trim(simpleTextList),
            listToSplit = Text.TrimEnd(Text.TrimStart(trimWhitespace, "{"), "}"),
            Result = List.Transform(Text.Split(listToSplit, ","), each Text.Trim(_))
        in
            Result
),
List.ToText = Document(
    "List.ToText",
    "Converts a list to a textual representation. Inverse of List.From",
    { [ Description = "Conver to text", Code = "List.ToText({ 1, 2, 3})", Result = """{1, 2, 3}"""] },
    (list as list) =>
            List.Accumulate(list, "{", (state, current) => current & Text.From(current)) & "}"
),
/////////////////////////
// Number              //
/////////////////////////
Number.Digits = {0,1,2,3,4,5,6,7,8,9},
Number.ParseText = (text as text, optional startIndex as number, optional allowCharacters as list) => 
    let
        consider = if startIndex is null then text else Text.Range(text,startIndex), 
        _allowCharacters = if allowCharacters is null then {} else allowCharacters,
        numberSeries = List.FirstN(List.Skip(Text.ToList(consider), each not Text.IsNumber(_)), each Text.IsNumber(_) or List.Contains(_allowCharacters, _))
    in 
        if text is null then null else Text.FromList(numberSeries),

/////////////////////////
// Splitters           //
/////////////////////////
Splitter.SplitTextByNonAlpha = (line as text) => Splitter.SplitTextByNotIn(Text.Alphabet),
Splitter.SplitTextByNotIn = (safeCharacters as text) => (line as nullable text) =>
  if line is null then {} else
  List.Accumulate(Text.ToList(line), {null} , (state, current) => 
    let
      doSkip = not Text.Contains(safeCharacters, current),
      lastItem = List.Last(state),
      appendLast = lastItem<>null
    in
      if doSkip then 
        if lastItem is null then 
          state 
        else 
          List.Combine({state, {null}})
      else
        if appendLast then
          List.Combine({List.RemoveLastN(state, 1), {lastItem & current}})
        else  
          List.Combine({List.RemoveLastN(state, 1), {current}})),


/////////////////////////
// Switch             //
////////////////////////

Switch = 
    Document(
        "Switch", 
        "Given a value, find it's paired item <br>"&
            "Switch(value as any, cases as list, results as list, optional default as any) <br>" &
            "Switch(value as any, pairs as list, optional default as any)",
        { 
            [ Description = "Using separate lists", Code = "Switch(1, {1, 2, 3}, {""A"", ""B"", ""C""})", Result = "A"],
            [ Description = "Using one paired list", Code = "Switch(1, {{1, ""A""}, {2, ""B""}, {3, ""C""}})", Result = "A"]
        },
        (value as any, casesOrPairs as list, optional resultsOrDefault as any, optional default as any) =>
            let
               hasPairs = List.First(casesOrPairs) is list,
               usingPairs = 
                   let
                       targetPosition = List.PositionOf(casesOrPairs, value, Occurrence.First, (case, theValue) => theValue = case{0})
                   in
                       if targetPosition = -1 then resultsOrDefault else casesOrPairs{targetPosition}{1},
               usingCases = 
                   let
                       cases = casesOrPairs,
                       results = resultsOrDefault
                   in
                       if List.IsEmpty(cases) or List.IsEmpty(results) then default else if value = List.First(cases) then List.First(results) else @Switch(value, List.Skip(cases, 1), List.Skip(results, 1), default)
            in
                if hasPairs then usingPairs else usingCases
    ),

///////////////////////// 
// Table               //
/////////////////////////


Table.AddBlankRow = (table as table) => Table.Combine({table, Table.FromRecords({[]})}),
// Transforms a column's value into its nested value -- if it eventually finds only one. Consider the following column:
//  MyCol
//  -----
//  "a"
//  {{{"b"}}}
//  Table.FromRecords({[MyCol=Table.FromRecords({[col=2]})})
//  {}
//
// Table.DrillColumn(table, "MyCol") will convert it to
//
//  MyCol
//  -----
//  "a"
//  "b"
//  2
//  null
Table.DrillIntoColumn = (table as table, columnName as text) =>
      let
          FindValue = (value as any) => 
            if value is list then
               if List.Count(value) = 1 then @FindValue(List.First(value)) 
               else if List.Count(value) = 0 then null
               else error "Couldn't find single value"
            else if value is table then
               if Table.RowCount(value) = 1 then @FindValue(List.First(Table.ToColumns(value)))
                                        else if Table.RowCount(value) = 0 then null 
               else error "Couldn't find single value"
            else  value,
          Result = Table.TransformColumns(table, {{columnName, FindValue}})
      in
         Result,
    
Table.EnterDataFormula = 
    Document(
        "Table.EnterDataFormula",
        "Convert a table into a formula that's compatible with the Enter Data UI. This function returns an expression you can copy and paste to editthe " &
            " table using Enter Data. Paste the formula, then click the gear icon next to the step name. This is an easy way to allow people to customize a table you've queried",
        {[ 
            Description = "Simple table", 
            Code = "Web.Page(Web.Contents(""https://en.wikipedia.org/wiki/List_of_Super_Bowl_champions"")){3}[Data]", 
            Result = "Table.FromRows(Json.Document(Binary.Decompress(Binary.FromText(""jVVtb9s2EP4rRD4aRqI3S9TH2Gm7ZWkb1MGGom6Aq83IhBkyoOik2a8fT6Ski6Fu+yDdWea9Pffc8du3M342P7uVzrU/jrbZs7UTQgnb+q+lfzL/nFeLxIu0ror55pglWcK8viA6J3o96vVwJksSqvNRT5Ognn2fh1yuQClo2cq8/DCvmMbCPzmmUWaLkEYyn2GkNIhFEFUQvBN1FkQexGI2uP8kXtg73SjQO3YLzkrjMEgRn/NFEmrlpL66JLmnRM+JXhC9IvWR82lxWqvQz8KypTV6a9pYJxZ5nlex1mpMg5dEp98p5OR7TWEmqaYLkgY2eQ2avbegt7LdGlbUof1dApgLz/MASQCcF0EEpH3wDuJiNkfXWUAabT9YITRbwqvHeXsIPovBZ4S5jDWl3kdZjbqHfBbKmXUkGd1iA78ae2AfJGg36ZRH46QzTqKIrtLR1Wc4IA82FzemZZe68cRv2ReQu5Bs3vO/7JOt5kixEt886d45Vp1kg8u/oN1L3Tij2Rexaw9ST3qqAj95FIGmPFC4Jhl+lPAo2ZVRT/vgKeuHoeg9pWT2MqLnhCDkO6ccROSWoJx8NFZsLn7XO48pPBklcQCV6yNmdDJKPrbJj+LIejol9UmUj1Jr0RoH7E/pQWnQc9LPXNJ7ronngEkVyFaVszHh48MDKMOWUqlJN3VCZoDAUxMY6pwkiIASCmwu1u6c3ZijRDY8tpFeHQh5nIUq0r4O3Iotw//WApxTgnm5h5fDhHW3DGdhKDtRBOuMdsNHfhZDy7vCYoEdo8dhzkghePQP0C3uT+le2WovxUOfQfqmieX8Hkeuvh8M/eEtNB5XAXbShoesk3JM93YvFewE0hPYO2g8ejHbjHaExxEkha6kXzdag5M+oG5ATRuS9nF+UukKrGeqBr9ftNuHmT3xgDs6LKZ4BaR0g1wL1xdKAUaK3w+n7+DxCbo1tjxut6CFsFNGuAXehvhslfC9YGuQejIOjslogkv4SgrfgNUebDOWk77hNh1f/OfS+RXmx+o9qK3R0zZ8jHIncA5bIdiddPCL8/VpDCv/Nh5nD/jO460mrPBSD1Hw10qJZ9Fdsf5qexmi4IOuUxzXT5uLy8HgSjhrpGM30vyP09f+PmmNfvYLQLBraI5g/9voN3NscS3fiZ/wbzG+/wM="", BinaryEncoding.Base64), Compression.Deflate)), let _t = ((type text) meta [Serialized.Text = true]) in type table [Appearances = _t, Team = _t, Wins = _t, Losses = _t, Winning 
percentage = _t, Season(s) = _t])"
        ]},
        (table as table) =>
            let
                Encoded = Table.JsonEncode(table),
                ColumnMeta= "[" & Text.Range(List.Accumulate(Table.ColumnNames(table), "", (state, current) => state & ", " & current & " = _t"), 2) & "]",
                Text = 
                    "Table.FromRows(Json.Document(Binary.Decompress(Binary.FromText(""" & Encoded & """, BinaryEncoding.Base64), Compression.Deflate))," & 
                    " let _t = ((type text) meta [Serialized.Text = true]) in type table " & ColumnMeta & ")"
            in
                Text
    ),
// if fieldNames aren't specified, use the field names from the first row of the column.
Table.ExpandRecordColumn = (table as table, columnName as text, optional fieldNames as list, optional newColumnNames as nullable list) => 
  let
    _fieldNames = if fieldNames <> null then fieldNames else List.Buffer(Record.FieldNames(List.First(Table.Column(table, columnName)))),
    _newColumnNames = if newColumnNames <> null then newColumnNames else _fieldNames,
    Result = Table.ExpandRecordColumn(table, columnName, _fieldNames , _newColumnNames)
  in 
    Result,

// Perform a cross join of lists. Example usage:
// Table.FromListCrossJoin({ {ColorsTable[ColorName], "Color"}, {NumbersTable[Number], "Number"}})
// Will give me a new table with two columns, "Color" and "Number" which contains one row for each possible
// combination of colors and numbers
// Table.FromListCrossJoin({{"Red", "Blue"}, "Color"}, {{1,2,3}, "Number"}}) = 
//    Table.FromRecords({[Color="Red", Number=1],[Color="Red", Number = 2],[Color="Red", Number = 3],[Color="Blue", Number=1],[Color="Blue", Number=2],[Color="Blue", Number=3]})
Table.FromListCrossJoin = (listColumnNamePairs as any) =>
      let 
       remainingPairs = List.Skip(listColumnNamePairs, 1),
       current = List.First(listColumnNamePairs),
       theList = List.First(current),
       columnName = List.First(List.Skip(current),1),
       firstTable = Table.FromList(theList, null, {columnName}),
       doStuff = (table as table, remainingPairs as list) =>
          if List.Count(remainingPairs) <= 0 then table else
          let 
             current = List.First(remainingPairs),
             theList = List.First(current),
             columnName = List.First(List.Skip(current), 1),
             nextTable = Table.ExpandListColumn(Table.AddColumn(table, columnName, each theList), columnName)
          in @doStuff(nextTable, List.Skip(remainingPairs, 1)),
       Result = doStuff(firstTable, remainingPairs)
   in
       Result,

Table.JsonDecode = (encoded as text) =>
    let
        Decompressed = Binary.Decompress(Binary.FromText(encoded, BinaryEncoding.Base64), Compression.Deflate),
        Decoded = Table.FromRows(Json.Document(Decompressed), let _t = ((type text) meta [Serialized.Text = true]) in type table [Name = _t, Number = _t])
    in
        Decoded,
Table.JsonEncode = (table as table) =>
    let
        Rows = Table.ToRows(table),
        Json = Json.FromValue(Rows),
        Compressed = Binary.Compress(Json, Compression.Deflate),
        Encoded = Binary.ToText(Compressed, BinaryEncoding.Base64)
    in
        Encoded,

// Replaces a value if it matches a predicate
Table.ReplaceValueIf = (table as table, replaceIf as function, after as any, columnNameOrList as any) => 
    Table.ReplaceValue(table, null,after, (text, old, new)=>if replaceIf(text) then new else text, if columnNameOrList is list then columnNameOrList else {columnNameOrList}),

// Splits camelCased and PascalCased column names. 
Table.SplitColumnNames = (table as table) => Table.RenameColumns(table, List.Transform(Table.ColumnNames(table), each {_, Text.SplitCamelCase(_)})), 

// Splits camelCased and PascalCased text in a column. 
Table.SplitColumnText = (table as table, columns as list) => if List.Count(columns) = 0 then table else Table.TransformColumns(@Table.SplitColumnText(table, List.Skip(columns, 1)), {{List.First(columns), Text.SplitCamelCase}}),

Table.TransformColumn = (table as table, column as text, transform as function) => Table.TransformColumns(table, {{column, transform}}),
Table.RenameColumn = (table as table, column as text, newName as text) => Table.RenameColumns(table, {{column, newName}}),
Table.RenameAndTransformColumn = (table, currentName as text, newName as text, transform as function) => Table.TransformColumn(Table.RenameColumns(table, {currentName, newName}), newName, transform),

///////////////////////// 
// Text                //
/////////////////////////


Text.Alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz",
Text.AlphaNumeric = Text.Alphabet & "0123456789",
Text.FromList = (list as list) => List.Accumulate(list, "", (state, current) => state & Text.From(current)),
// Is text all uppercase? returns false if any non-alpha characters are present
Text.IsUpperCase = (text as text) => List.AllTrue(List.Transform(Text.ToList(text), (letter)=>Text.Contains(Text.Alphabet, letter) and letter = Text.Upper(letter))),
Text.IsAlpha = (text as text) => List.MatchesAll(Text.ToList(text), each Text.Contains(Text.Alphabet, _)),
Text.RemoveExtraWhitespace = (text as text) => Text.Combine(Splitter.SplitTextByWhitespace()(text)," "), 
// Splits camelCased and PascalCased text and separates by a space. Ex: "thisIsAColumn" -> "this Is A Column"
Text.SplitCamelCase = (text as nullable text) => if text is null then null else List.Accumulate(Text.ToList(text),"", (state, current) => 
   let
      PreviousLetter = Text.End(state, 1),
      Ignore = (text as text) => text = " " or text = "."
   in 
      state & 
      (if 
         not Text.IsUpperCase(PreviousLetter) and 
         not Ignore(PreviousLetter) and 
         not Ignore(current) and 
         Text.IsUpperCase(current) 
       then 
         " " else "" ) & 
      current),

Text.SplitOnNotIn = (line as nullable text, validCharacters as text) => Splitter.SplitTextByNotIn(validCharacters)(line),

Text.SplitOnNonAlpha = (line as nullable text) =>
  if line is null then null else List.Accumulate(Text.ToList(line), {null} , (state, current) => 
    let
      doSkip = not Text.Contains(Text.Alphabet, current),
      lastItem = List.Last(state),
      appendLast = lastItem<>null
    in
      if doSkip then 
        if lastItem is null then 
          state 
        else 
          List.Combine({state, {null}})
      else
        if appendLast then
          List.Combine({List.RemoveLastN(state, 1), {lastItem & current}})
        else  
          List.Combine({List.RemoveLastN(state, 1), {current}})),


Text.Substring = (text as text, start as number, optional count as number) => 
   let 
      start = if start >= 0 then start else error "start index should be >= 0",
      end = if count = null then Text.Length(text) else if count <= Text.Length(text) then count else error "count should be <= text length",
      textList = Text.ToList(text),
      substr = Text.FromList(List.FirstN(List.Skip(textList, start), end - start))
   in substr,

Text.IsNumber = (text as text) => try Number.FromText(text) is number otherwise false,
Text.PositionAfter = (text as nullable text, substring as text) => 
    let 
        firstIndex = Text.PositionOf(text, substring),
        indexAfter = if firstIndex >=0 then firstIndex + Text.Length(substring) else -1
    in
        if text is null then -1 else if indexAfter >= 0 and indexAfter < Text.Length(text) then indexAfter else -1,
Text.Until = (text as text, endDelimiter as text, optional startIndex as number) => 
    let
        start = if startIndex = null then 0 else startIndex,
        textFromStart = Text.Substring(text, start),
        delimPosition = if Text.PositionOf(textFromStart, endDelimiter) >= 0 then Text.PositionOf(textFromStart, endDelimiter) else Text.Length(textFromStart)
    in
        if text is null then null else Text.Range(textFromStart, 0, delimPosition),


///////////////////////// 
// Value               //
/////////////////////////

Value.TypeText = (value as any) => 
    if value is binary then "binary" else
    if value is date then "date" else
    if value is datetime then "datetime" else
    if value is datetimezone then "datetimezone" else
    if value is duration then "duration" else
    if value is function then "function" else
    if value is list then "list" else
    if value is logical then "logical" else
    if value is none then "none" else
    if value is null then "null" else
    if value is number then "number" else
    if value is record then "record" else
    if value is table then "table" else
    if value is text then "text" else
    if value is time then "time" else
    if value is type then "type" else
    if value is any then "any"
    else error "unknown -- not a primitive type!"

],
Result = _extensionLibrary
in
  Result
