=let 
PQExtentions =
 [
 Alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz",
 //Get a list of all dates between two end points
 DatesBetween= (start as any, end as any) => 
      let 
        StartDate = Date.From(start), 
        EndDate = Date.From(end),
        adjustedStart = List.Min({StartDate, EndDate}),
        adjustedEnd = List.Max({StartDate, EndDate}),
        GetDates = (start as date, end as date, dates as list)=> if start > end then dates else @GetDates(Date.AddDays(start, 1), end, List.Combine({dates, {start}})),
        Dates = GetDates(adjustedStart, adjustedEnd, {})
      in Dates,
 // Is text all uppercase? returns false if any non-alpha characters are present
 IsUpperCase = (text as text) => List.AllTrue(List.Transform(Text.ToList(text), (letter)=>Text.Contains(Alphabet, letter) and letter = Text.Upper(letter))),
 // Splits camelCased and PascalCased text and separates by a space. Ex: "thisIsAColumn" -> "this Is A Column"
 SplitCamelCase = (text as nullable text) => if text is null then null else List.Accumulate(Text.ToList(text),"", (state, current) => let
       PreviousLetter = Text.End(state, 1),
       Ignore = (text as text) => text = " " or text = "."
       in state & (if not IsUpperCase(PreviousLetter) and not Ignore(PreviousLetter) and not Ignore(current) and IsUpperCase(current) then " " else "" )& current),
 // Splits camelCased and PascalCased column names. 
 SplitColumnNames = (table as table) => Table.RenameColumns(table, List.Transform(Table.ColumnNames(table), each {_, SplitCamelCase(_)})), 
 // Splits camelCased and PascalCased text in a column. 
 SplitColumnText = (table as table, columns as list) => if List.Count(columns) = 0 then table else Table.TransformColumns(@SplitColumnText(table, List.Skip(columns, 1)), {{List.First(columns), SplitCamelCase}})
 ]
in PQExtentions
