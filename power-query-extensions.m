= [

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
// Splitters           //
/////////////////////////
Splitter.SplitTextByNonAlpha = (line as text) => 
	List.Accumulate(Text.ToList(line), {null} , (state, current) => 
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
					List.Combine({List.RemoveLastN(state, 1), {lastItem&current}})
				else 	
					List.Combine({List.RemoveLastN(state, 1), {current}})),
Splitter.SplitTextByNotIn = (safeCharacters as text) => (line as text)=>
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
					List.Combine({List.RemoveLastN(state, 1), {lastItem&current}})
				else 	
					List.Combine({List.RemoveLastN(state, 1), {current}})),

///////////////////////// 
// Text                //
/////////////////////////
Text.Alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz",
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

Text.SplitOnNonAlpha = (line as text) =>
	List.Accumulate(Text.ToList(line), {null} , (state, current) => 
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
					List.Combine({List.RemoveLastN(state, 1), {lastItem&current}})
				else 	
					List.Combine({List.RemoveLastN(state, 1), {current}})),


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

// Perform a cross join of lists. Example usage:
// Table.FromListCrossJoin({ {ColorsTable[ColorName], "Color"}, {NumbersTable[Number], "Number"}})
// Will give me a new table with two columns, "Color" and "Number" which contains one row for each possible
// combination of colors and numbers
// Table.FromListCrossJoin({{"Red", "Blue"}, "Color"}, {{1,2,3}, "Number"}}) = Table.FromRecords({[Color="Red", Number=1],[Color="Red", Number = 2],[Color="Red", Number = 3],[Color="Blue", Number=1],[Color="Blue", Number=2],[Color="Blue", Number=3]})
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

// Replaces a value if it matches a predicate
Table.ReplaceValueIf = (table as table, replaceIf as function, after as any, columnNameOrList as any) => Table.ReplaceValue(table, null,after, (text, old, new)=>if replaceIf(text) then new else text, if columnNameOrList is list then columnNameOrList else {columnNameOrList}),

// Splits camelCased and PascalCased column names. 
Table.SplitColumnNames = (table as table) => Table.RenameColumns(table, List.Transform(Table.ColumnNames(table), each {_, Text.SplitCamelCase(_)})), 

// Splits camelCased and PascalCased text in a column. 
Table.SplitColumnText = (table as table, columns as list) => if List.Count(columns) = 0 then table else Table.TransformColumns(@Table.SplitColumnText(table, List.Skip(columns, 1)), {{List.First(columns), Text.SplitCamelCase}}),


///////////////////////// 
// Misc.               //
/////////////////////////
Switch = (value as any, cases as list, results as list, optional default as any) => if List.IsEmpty(cases) or List.IsEmpty(results) then default else if value = List.First(cases) then List.First(results) else @Switch(value, List.Skip(cases, 1), List.Skip(results, 1), default)    

]
