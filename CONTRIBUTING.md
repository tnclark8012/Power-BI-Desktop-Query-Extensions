# How can I contribute? 
Pull requests are the best way to contribute changes. The following must be included in all pull requests for them to be considered:

## Do - Document your function
Use the Document function in the library to add the name, description, and examples to your function's implementation.
~~~~
Date.MonthName = Document(
    "Your function name",
    "Description of your function",
    {[ Description = "Example 1 description", Code="PBI\[YourFunction\](...)", Result = "The result of 'Code'" ]},
    () => ... // your function implementation
)
~~~~

## Do - Test your function
With minimal exception, all pull requests should include testing for the changes. Tests have the following form:
~~~~
[ 
        CaseName = "YourFunctionName - Extra description if necessary", 
        Test = (library as record) => 
            TestUtils[AssertEqual](
                "expected value", 
                library[YourFunctionName]()/* the code to excute when running the test*/,
                "Failure output")
]
~~~~
To run your test, simply click on the TestResults step



## Do - Alphabetize your test and function
Keeping function and test declarations alphabetical allows users and contributors to quickly find functions

## Consider - Handling null inputs
Consider returning null, or another meaningful value if an expected input is null.

## Consider - Throwing meaningful errors
If preconditions aren't met, throw a proper error. You can find info about throw throw errors in M here: https://msdn.microsoft.com/en-us/library/mt211003.aspx
