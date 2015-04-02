let 
Test = PQX[Date.MonthName](1),
PQX =[

///////////////////////// 
// Date                //
/////////////////////////
// Basic calendar
Date.Calendar = (start as any, end as any) => 
	let
    		Source = pqx[Date.DatesBetween](StartDate, EndDate),
    		FromList = Table.FromList(Source, Splitter.SplitByNothing(), null, null, ExtraValues.Error),
    		Date = Table.RenameColumns(FromList,{{"Column1", "Date"}}),
    		DayOfWeek = Table.AddColumn(Date, "Day of Week", each pqx[Date.DayName]([Date])),
    		Month = Table.AddColumn(DayOfWeek, "Month", each pqx[Date.MonthName]([Date])),
    		WeekStartDate = Table.AddColumn(Month, "WeekStartDate", each Date.StartOfWeek([Date])),
    		WeekStart = Table.AddColumn(WeekStartDate, "Week Start", each [Month] & " " & Text.From(Date.Day([WeekStartDate])))
	in
    		WeekStart,
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
// Text                //
/////////////////////////
Text.Alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz",
Text.FromList = (list as list) => List.Accumulate(list, "", (state, current) => state & Text.From(current)),
// Is text all uppercase? returns false if any non-alpha characters are present
Text.IsUpperCase = (text as text) => List.AllTrue(List.Transform(Text.ToList(text), (letter)=>Text.Contains(Text.Alphabet, letter) and letter = Text.Upper(letter))),

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
Text.Substring = (text as text, start as number, optional count as number) => 
	let 
		start = if start >= 0 then start else error "start index should be >= 0",
		end = if count <= Text.Length(text) then count else error "count should be <= text length",
		textList = Text.ToList(text),
		substr = Text.FromList(List.FirstN(List.Skip(textList, start), end - start))
	in substr,

///////////////////////// 
// Table               //
/////////////////////////
// Splits camelCased and PascalCased column names. 
Table.SplitColumnNames = (table as table) => Table.RenameColumns(table, List.Transform(Table.ColumnNames(table), each {_, Text.SplitCamelCase(_)})), 

// Splits camelCased and PascalCased text in a column. 
Table.SplitColumnText = (table as table, columns as list) => if List.Count(columns) = 0 then table else Table.TransformColumns(@Table.SplitColumnText(table, List.Skip(columns, 1)), {{List.First(columns), Text.SplitCamelCase}}),


///////////////////////// 
// Misc.               //
/////////////////////////
Switch = (value as any, cases as list, results as list, default as any) => if List.IsEmpty(cases) or List.IsEmpty(results) then default else if value = List.First(cases) then List.First(results) else @Switch(value, List.Skip(cases, 1), List.Skip(results, 1), default)    



]
in
    PQX
