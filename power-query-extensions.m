let 
PQExtentions =
 [
 //
 // Text
 // 
 Alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz",
 // Is text all uppercase? returns false if any non-alpha characters are present
 IsUpperCase = (text as text) => List.AllTrue(List.Transform(Text.ToList(text), (letter)=>Text.Contains(Alphabet, letter) and letter = Text.Upper(letter))),
 // Splits camelCased and PascalCased text and separates by a space. Ex: "thisIsAColumn" -> "this Is A Column"
 
 //
 // Table
 //
 SplitCamelCase = (text as nullable text) => if text is null then null else List.Accumulate(Text.ToList(text),"", (state, current) => let
       PreviousLetter = Text.End(state, 1),
       Ignore = (text as text) => text = " " or text = "."
       in state & (if not IsUpperCase(PreviousLetter) and not Ignore(PreviousLetter) and not Ignore(current) and IsUpperCase(current) then " " else "" )& current),
 // Splits camelCased and PascalCased column names. 
 SplitColumnNames = (table as table) => Table.RenameColumns(table, List.Transform(Table.ColumnNames(table), each {_, SplitCamelCase(_)})), 
 // Splits camelCased and PascalCased text in a column. 
 SplitColumnText = (table as table, columns as list) => if List.Count(columns) = 0 then table else Table.TransformColumns(@SplitColumnText(table, List.Skip(columns, 1)), {{List.First(columns), SplitCamelCase}}),
 
//
// Dates
//
// Dates between. Start and end can be flipped
DatesBetween= (start as any, end as any) => 
      let 
        StartDate = Date.From(start), 
        EndDate = Date.From(end),
        adjustedStart = List.Min({StartDate, EndDate}),
        adjustedEnd = List.Max({StartDate, EndDate}),
        GetDates = (start as date, end as date, dates as list)=> if start > end then dates else @GetDates(Date.AddDays(start, 1), end, List.Combine({dates, {start}})),
        Dates = GetDates(adjustedStart, adjustedEnd, {})
      in Dates,
// Sunday is 0
DayOfWeek = (dayOfWeekIndex as number) => Switch(dayOfWeekIndex, {0, 1, 2, 3, 4, 5, 6}, {"Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"}, null),
MonthName  = (monthNum as number) => Switch(monthNum, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12}, 
        {"January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"}, null),
 
 //
 // Utility
 //
 Switch = (value as any, cases as list, results as list, default as any) => if List.IsEmpty(cases) or List.IsEmpty(results) then default else if value = List.First(cases) then List.First(results) else @Switch(value, List.Skip(cases, 1), List.Skip(results, 1), default)    
 ]
in PQExtentions
