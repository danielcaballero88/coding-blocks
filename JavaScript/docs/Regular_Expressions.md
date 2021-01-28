# Regular Expressions in JavaScript 

## Notation

To indicate a regex use "/": /<regex>/ 

## Test

To test if a regex is found in a string: /<regex>/.text('<string>')

    let testStr = "freeCodeCamp";
    let testRegex = /Code/;
    testRegex.test(testStr);
    // Returns true

## String literal regex

If the regex is a string literal, then the exact match is checked

    let testStr = "Hello, my name is Kevin.";
    let testRegex = /Kevin/;
    testRegex.test(testStr);
    // Returns true

    let wrongRegex = /kevin/;
    wrongRegex.test(testStr);
    // Returns false



## Match

Instead of returning true or false, the match method returns the occurrences:

    "Hello, World!".match(/Hello/);
    // Returns ["Hello"]
    let ourStr = "Regular expressions";
    let ourRegex = /expressions/;
    ourStr.match(ourRegex);

## Flags

The i flag sets to ignore case:

    let wrongRegex = /kevin/i;
    wrongRegex.test(testStr);
    // Returns true

    let testStr = "Repeat, Repeat, Repeat";
    let ourRegex = /Repeat/;
    testStr.match(ourRegex);
    // Returns ["Repeat"]

The g flag returns all possibilities

    let repeatRegex = /Repeat/g;
    testStr.match(repeatRegex);
    // Returns ["Repeat", "Repeat", "Repeat"]

## Wildcard (period or dot)

The wildcard character . will match any one character.

    let humStr = "I'll hum a song";
    let hugStr = "Bear hug";
    let huRegex = /hu./;
    huRegex.test(humStr); // Returns true
    huRegex.test(hugStr); // Returns true

## Character sets (or classes?)

### Character by character

Character sets allow you to define a group of characters you wish to match by placing them inside square ([ and ]) brackets.

    let bigStr = "big";
    let bagStr = "bag";
    let bugStr = "bug";
    let bogStr = "bog";
    let bgRegex = /b[aiu]g/;
    bigStr.match(bgRegex); // Returns ["big"]
    bagStr.match(bgRegex); // Returns ["bag"]
    bugStr.match(bgRegex); // Returns ["bug"]
    bogStr.match(bgRegex); // Returns null

### Character range

To avoid listing all the characters, you can define a range of characters to match using a hyphen character: -.

    let catStr = "cat";
    let batStr = "bat";
    let matStr = "mat";
    let bgRegex = /[a-e]at/;
    catStr.match(bgRegex); // Returns ["cat"]
    batStr.match(bgRegex); // Returns ["bat"]
    matStr.match(bgRegex); // Returns null

### Combine letters and numbers

Also, it is possible to combine a range of letters and numbers in a single character set.

    let jennyStr = "Jenny8675309";
    let myRegex = /[a-z0-9]/ig;
    // matches all letters and numbers in jennyStr
    jennyStr.match(myRegex);

### Search all letters and numbers

This kind of character class is common enough that there is a shortcut for it.
The closest character class in JavaScript to match the alphabet is `\w`.
This shortcut is equal to `[A-Za-z0-9_]` (it also includes the underscore character `_`).

    let longHand = /[A-Za-z0-9_]+/;
    let shortHand = /\w+/;
    let numbers = "42";
    let varNames = "important_var";
    longHand.test(numbers); // Returns true
    shortHand.test(numbers); // Returns true
    longHand.test(varNames); // Returns true
    shortHand.test(varNames); // Returns true

These shortcut character classes are also known as _shorthand character classes_.

### Match everything BUT letters and numbers

You can search for the opposite of the `\w` with `\W`. 
Note, the opposite pattern uses a capital letter. 
This shortcut is the same as `[^A-Za-z0-9_]`.

    let shortHand = /\W/;
    let numbers = "42%";
    let sentence = "Coding!";
    numbers.match(shortHand); // Returns ["%"]
    sentence.match(shortHand); // Returns ["!"]

This set includes the whitespace, dot, etc...

### Match all numbers

The shortcut to look for digit characters is `\d`.

    let movieName = "2001: A Space Odyssey";
    let numRegex = /\d/g; // Change this line
    let result = movieName.match(numRegex);
    console.log(result)
    // ['2', '0', '0', '1']

### Match all non-numbers

The shortcut to look for non-digit characters is `\D`.
This is equal to the character class `[^0-9]`.

    let movieName = "2001: A Space Odyssey";
    let noNumRegex = /\D/g; // Change this line
    let result = movieName.match(noNumRegex);
    console.log(result)
    [':', ' ', 'A', ' ', 'S', 'p', 'a', 'c', 'e', ' ', 'O', 'd', 'y', 's', 's', 'e', 'y']

## Negated character sets 

To create a negated character set, you place a caret character (^) after the opening bracket and before the characters you do not want to match.

    let quoteSample = "3 blind mice.";
    let myRegex = /[^aeiou0-9]/ig; // Change this line
    let result = quoteSample.match(myRegex); // Change this line
    console.log(result)
    // returns [ ' ', 'b', 'l', 'n', 'd', ' ', 'm', 'c', '.' ]

## Match Characters Repeat (consecutively)

### One or more times

You can use the + character to check if that is the case.

    let difficultSpelling = "Mississippi";
    let myRegex = /s+/ig; // Change this line
    let result = difficultSpelling.match(myRegex);
    // returns [ 'ss', 'ss' ]

### Zero or more times

The character to do this is the asterisk or star: \*.

    let soccerWord = "gooooooooal!";
    let gPhrase = "gut feeling";
    let oPhrase = "over the moon";
    let goRegex = /go*/;
    soccerWord.match(goRegex); // Returns ["goooooooo"]
    gPhrase.match(goRegex); // Returns ["g"]
    oPhrase.match(goRegex); // Returns null

## Lazy Matching

In regular expressions, a greedy match finds the longest possible part of a string that fits the regex pattern and returns it as a match.

You can apply the regex `/t[a-z]*i/` to the string `"titanic"`.

Regular expressions are by default greedy, so the match would return `["titani"]`.

However, you can use the `?` character to change it to lazy matching. `"titanic"` matched against the adjusted regex of `/t[a-z]*?i/` returns `["ti"]`.

Greedy:

    let text = "<h1>Winter is coming</h1>";
    let myRegex = /<.*>/; // Change this line
    let result = text.match(myRegex);
    // returns [ '<h1>Winter is coming</h1>' ]

Lazy:

    let text = "<h1>Winter is coming</h1>";
    let myRegex = /<.*?>/; // Change this line
    let result = text.match(myRegex);
    // returns [ '<h1>' ]

## Match at the beginning or end of a string

Outside of a character set, the caret `^` is used to search for patterns at the beginning of strings.

    let firstString = "Ricky is first and can be found.";
    let firstRegex = /^Ricky/;
    firstRegex.test(firstString);
    // Returns true
    let notFirst = "You can't find Ricky now.";
    firstRegex.test(notFirst);
    // Returns false

You can search the end of strings using the dollar sign character `$` at the end of the regex.

    let theEnding = "This is a never ending story";
    let storyRegex = /story$/;
    storyRegex.test(theEnding);
    // Returns true
    let noEnding = "Sometimes a story will have to end";
    storyRegex.test(noEnding);
    // Returns false


