Red[]
;Tokening
IntegerDigit: func[i [integer!] content [string!] output-file [file!]][
    firstI: i
    ch: to-string pick content i
    while [to-logic find numbers ch] [
        i: i + 1
        ch: to-string pick content i
    ]
    write/append output-file rejoin ["<integerConstant> " copy/part at content firstI i - firstI  " </integerConstant>^/"]
    return i
]
StringChar: func[i[integer!] content[string!] output-file [file!]][
    firstI: i 
    ch: to-string pick content i
    while [ch <> "^""] [
        i: i + 1
        ch: to-string pick content i
    ]
    write/append output-file rejoin ["<stringConstant> " copy/part at content firstI i - firstI " </stringConstant>^/"]
    return i + 1
]
LineComment: func[i[integer!] content[string!] output-file [file!]][
    firstI: i + 1
    ch: to-string pick content i
    while [ch <> "^/"] [
        i: i + 1
        ch: to-string pick content i
    ]
    return i + 1
]
LinesComment: func[i[integer!] content[string!] output-file [file!]][
    firstI: i + 1
    ch: to-string pick content i
    while [ch <> "*" or ((to-string pick content (i + 1)) <> "/")] [
        i: i + 1
        ch: to-string pick content i
    ]
    return i + 2
]
KeyWordIdentifier: func[i[integer!] content[string!] output-file [file!]][
    firstI: i
    ch: to-string pick content i 
    while [ch = "_" or (to-logic find letters ch) or (to-logic find numbers ch) ] [
        i: i + 1
        ch: to-string pick content i
    ]
    either (to-logic find keyWords (copy/part at content firstI - 1 i - firstI + 1)) 
    [write/append output-file rejoin ["<keyword> " copy/part at content firstI - 1 i - firstI + 1 " </keyword>^/"]]
    [write/append output-file rejoin ["<identifier> " copy/part at content firstI - 1 i - firstI + 1 " </identifier>^/"]]
    return i 
]
Symbol: func[i [integer!] content [string!] output-file [file!]][
    ch: to-string pick content i
    case [
        ch = "<" [ch: "&lt;"]
        ch = ">" [ch: "&gt;"]
        ch = "&" [ch: "&amp;"]
        true []
    ]
    write/append output-file rejoin ["<symbol> " ch " </symbol>^/"] 
    return i + 1
]

;Parsing
findSymbol: func[varName [string!]][
    foreach symbolL (function-scope-symbol-table)[
        line: split symbolL " "
        if varName == pick line 1 [
            return line 
        ]
    ]
    foreach symbolL (class-scope-symbol-table)[
        line: split symbolL " "
        if varName == pick line 1 [
            return line 
        ]
    ]
    return null
]
getNextToken: func[][
    nextToken: pick allTokens i
    i: i + 1
    return nextToken
]
checkNextToken: func[][
    nextToken: pick allTokens i
    strToken: copy nextToken
    if equal? "<stringConstant>" pick copy split strToken " " 1 [
        replace strToken "<stringConstant> " ""
        replace strToken " </stringConstant>" ""
        return strToken
    ]
    token: copy split nextToken " "
    return pick token 2
]
ExpressionList: func[/local counter][
    counter: parameterCounter
    if checkNextToken <> ")" [
        Expression
        counter: counter + 1
        while[checkNextToken = ","][
            getNextToken ;symbol- ,
            Expression
            counter: counter + 1
        ]
    ]
    parameterCounter: counter
]
SubroutineCall: func[/local SCfunctionName name][
    SCfunctionName: checkNextToken
    getNextToken ;identifier- subroutine call name
    either checkNextToken = "." [
        getNextToken ;symbol- .
        name: checkNextToken
        getNextToken ;identifier- name
        SCfSymbol: findSymbol SCfunctionName
        either equal? SCfSymbol null [
            parameterCounter: 0
            getNextToken ;symbol- (
            ExpressionList parameterCounter
            getNextToken ;symbol- )
            write/append output-file rejoin["call " SCfunctionName "." name " " parameterCounter "^/"]
        ]
        [
            write/append output-file rejoin["push " pick SCfSymbol 3 " " pick SCfSymbol 4 "^/"]
            parameterCounter: 1
            getNextToken ;symbol- (
            ExpressionList parameterCounter
            getNextToken ;symbol- )
            write/append output-file rejoin["call " pick SCfSymbol 2 "." name " " parameterCounter "^/"]
        ] 
    ]
    [
        write/append output-file "push pointer 0^/"
        parameterCounter: 1
        getNextToken ;symbol- (
        ExpressionList parameterCounter
        getNextToken ;symbol- )
        write/append output-file rejoin["call " className "." SCfunctionName " " parameterCounter "^/"] 
    ]
    
]
Term: func[/local TnToken][
    keyWordConstant: ["true" "false" "null" "this"]
    TnToken: checkNextToken
    case [
        TnToken = "(" [getNextToken ;symbol- (
                    Expression
                    getNextToken] ;symbol- )
        to-logic find keyWordConstant TnToken [getNextToken ;key word constant
        str: null   
        case [
            TnToken = "true" [str: "push constant 0^/not^/"]
            TnToken = "false" or (TnToken = "null") [str: "push constant 0^/"]
            TnToken = "this" [str: "push pointer 0^/"]
        ]
        write/append output-file str]
        TnToken = "~" or (TnToken = "-") [getNextToken ;symbol- ~ or -
                                    Term
                                    either equal? TnToken "~" [write/append output-file "not^/"]
                                    [write/append output-file "neg^/"]]
        true [curToken: pick split getNextToken space 1 
            if equal? "<integerConstant>" curToken [
                write/append output-file rejoin ["push constant " TnToken "^/"]
            ]
            if equal? "<stringConstant>" curToken [
                len: length? TnToken
                write/append output-file rejoin["push constant " len "^/call String.new 1^/"]
                foreach ch (TnToken)[
                    write/append output-file rejoin["push constant " to-integer to-char ch "^/call String.appendChar 2^/"]
                ]
            ] 
            nextToken: checkNextToken
            case [
                nextToken = "." or (nextToken = "(") [i: i - 1 
                SubroutineCall]
                nextToken = "[" [getNextToken ;symbol- [
                            Expression
                            fSymbol: findSymbol TnToken
                            write/append output-file rejoin["push " pick fSymbol 3 " " pick fSymbol 4 "^/add^/pop pointer 1^/push that 0^/"]
                            getNextToken] ;symbol- ]
                curToken = "<identifier>" [fSymbol: findSymbol TnToken
                    write/append output-file rejoin["push " pick fSymbol 3 " " pick fSymbol 4 "^/"]]            
            ]
        ]
    ]
]
Expression: func[/local EnToken][
    op: ["+" "-" "*" "/" "&amp;" "|" "&lt;" "&gt;" "="]
    Term
    while [to-logic find op checkNextToken][
        EnToken: checkNextToken
        getNextToken ;symbol- op
        Term
        str: ""
        case [
            EnToken = "+" [str: "add^/"]
            EnToken = "-" [str: "sub^/"]
            EnToken = "*" [str: "call Math.multiply 2^/"]
            EnToken = "/" [str: "call Math.divide 2^/"]
            EnToken = "&amp;" [str: "and^/"]
            EnToken = "|" [str: "or^/"]
            EnToken = "&lt;" [str: "lt^/"]
            EnToken = "&gt;" [str: "gt^/"]
            EnToken = "=" [str: "eq^/"]
        ]
        write/append output-file str
    ]
]
ReturnStatement: func[][
    getNextToken ;keyword- return 
    either checkNextToken <> ";"[
        Expression
    ]
    [
        write/append output-file "push constant 0^/"
    ]    
    write/append output-file "return^/"    
    getNextToken ;symbol- ;
]
DoStatement: func [][
    getNextToken ;keyword- do
    SubroutineCall
    getNextToken ;symbol- ;
    write/append output-file "pop temp 0^/"
]
WhileStatement: func[][
    getNextToken ;keyword- while
    getNextToken ;symbol- (
    write/append output-file rejoin ["label WHILE_EXP" whileCounter "^/"]  
    Expression
    write/append output-file rejoin ["if-goto WHILE_TRUE" whileCounter "^/goto WHILE_FALSE" whileCounter "^/label WHILE_TRUE" whileCounter "^/"]
    append openWhile whileCounter
    whileCounter: whileCounter + 1
    getNextToken ;symbol- )   
    getNextToken ;symbol- {
    Statements
    getNextToken ;symbol- }
    write/append output-file rejoin ["goto WHILE_EXP" last openWhile "^/label WHILE_FALSE" last openWhile "^/"]
    remove-each this-item openWhile [this-item = last openWhile]
]
IfStatement: func[][
    getNextToken ;keyword- if
    getNextToken ;symbol- (
    Expression
    write/append output-file rejoin ["not^/if-goto IF_FALSE" ifCounter "^/"]
    getNextToken ;symbol- )   
    getNextToken ;symbol- { 
    append openIf ifCounter
    ifCounter: ifCounter + 1
    Statements
    getNextToken ;symbol- }
    either checkNextToken = "else" [
        write/append output-file rejoin["goto IF_END" last openIf "^/"]
        write/append output-file rejoin ["label IF_FALSE" last openIf "^/"]
            
        getNextToken ;keyword- else
        getNextToken ;symbol- {
        Statements
        getNextToken ;symbol- }
        write/append output-file rejoin["label IF_END" last openIf "^/"]
    ]
    [
        write/append output-file rejoin ["label IF_FALSE" last openIf "^/"]
    ]
    remove-each this-item openIf [this-item = last openIf]
]
LetStatement: func[][
    getNextToken ;keyword- Let
    varName: checkNextToken
    getNextToken ;identifier- var name
    LSfSymbol: findSymbol varName
    either checkNextToken = "[" [
        getNextToken ;symbol- [
        Expression
        write/append output-file rejoin["push " pick LSfSymbol 3 " " pick LSfSymbol 4 "^/add^/"]
        getNextToken ;symbol- ]
        getNextToken ;symbol- =
        Expression
        write/append output-file "pop temp 0^/pop pointer 1^/push temp 0^/pop that 0^/"
    ]
    [ 
        getNextToken ;symbol- =
        Expression
        write/append output-file rejoin["pop " pick LSfSymbol 3 " " pick LSfSymbol 4 "^/"]
    ]
    getNextToken ;symbol- ; 
]
Statements: func[][
    SnToken: checkNextToken
    arr: ["let" "if" "while" "do" "return"]
    while [to-logic find arr SnToken][
        switch SnToken[
            "let" [LetStatement]
            "if" [IfStatement]
            "while" [WhileStatement]
            "do" [DoStatement]
            "return" [ReturnStatement]
        ]
        SnToken: checkNextToken
    ]
]
VarDec: func[][
    while [checkNextToken = "var"][
        getNextToken ;keyword- var
        type: checkNextToken
        getNextToken ;keyword or idntifier- var type
        name: checkNextToken
        getNextToken ;identifier- var name
        append function-scope-symbol-table rejoin[name " " type " local " varsCounter]
        varsCounter: varsCounter + 1
        while [checkNextToken = ","][
            getNextToken ;symbol- ,
            name: checkNextToken
            getNextToken ;identifier- var name
            append function-scope-symbol-table rejoin[name " " type " local " varsCounter]
            varsCounter: varsCounter + 1
        ]
        getNextToken ;symbol- ;
    ]
]
SubrutineBody: func[][
    getNextToken ;symbol- {
    VarDec
    write/append output-file rejoin["function " className "." functionName " " varsCounter "^/"]
    if equal? functionType "method" [
        write/append output-file "push argument 0^/pop pointer 0^/"
    ]
    if equal? functionType "constructor" [
        write/append output-file rejoin ["push constant " fieldCounter "^/call Memory.alloc 1^/pop pointer 0^/"]
    ]
    Statements
    getNextToken ;symbol- }
]
ParameterList: func[][
    if checkNextToken <> ")" [
        type: checkNextToken
        getNextToken ;keyword or idntifier- parameter type
        name: checkNextToken
        getNextToken ;identifier- parameter name
        append function-scope-symbol-table rejoin[name " " type " argument " argumentCounter]
        argumentCounter: argumentCounter + 1
        while [checkNextToken = ","][
            getNextToken ;symbol- ,
            type: checkNextToken
            getNextToken ;keyword or idntifier- parameter type
            name: checkNextToken
            getNextToken ;identifier- parameter name
            append function-scope-symbol-table rejoin[name " " type " argument " argumentCounter]
            argumentCounter: argumentCounter + 1
        ]
    ]
]
SubDec: func[][
    SDnToken: checkNextToken
    while [(SDnToken = "constructor") or (SDnToken = "function") or (SDnToken = "method")] [
        function-scope-symbol-table: copy []
        argumentCounter: 0
        varsCounter: 0
        functionType: checkNextToken
        getNextToken ;keyword- constructor or function or method
        returnType: checkNextToken
        getNextToken ;keyword or idntifier- return type
        functionName: checkNextToken
        getNextToken ;identifier- function name
        if equal? SDnToken "method" [
            append function-scope-symbol-table rejoin["this " className " argument " argumentCounter]
            argumentCounter: argumentCounter + 1
        ]
        getNextToken ;symbol- (
        ParameterList 
        getNextToken ;symbol- )
        SubrutineBody
        SDnToken: checkNextToken
    ]
]
ClassVarDec: func[][
    CVDnToken: checkNextToken
    while [(CVDnToken = "static") or (CVDnToken = "field")] [
        kind: checkNextToken
        getNextToken ;keyword- static or field
        type: checkNextToken
        getNextToken ;keyword- type
        name: checkNextToken
        getNextToken ;identifier- name
        either equal? kind "static" [
            append class-scope-symbol-table rejoin[name " " type " static " staticCounter]
            staticCounter: staticCounter + 1
        ]
        [
            append class-scope-symbol-table rejoin[name " " type " this " fieldCounter]
            fieldCounter: fieldCounter + 1  
        ]
        while [checkNextToken = ","][
            getNextToken ;symbol- ,
            name: checkNextToken
            getNextToken ;identifier- name
            either equal? kind "static" [
                append class-scope-symbol-table rejoin[name " " type " static " staticCounter]
                staticCounter: staticCounter + 1
            ]
            [
                append class-scope-symbol-table rejoin[name " " type " this " fieldCounter]
                fieldCounter: fieldCounter + 1  
            ]
        ]
        getNextToken ;symbol- ;
        CVDnToken: checkNextToken
    ]
]
ParseClass: func[][
    class-scope-symbol-table: copy []
    staticCounter: 0
    fieldCounter: 0
    ifCounter: 0
    whileCounter: 0
    openWhile: []
    openIf: []
    getNextToken ;keyword- class
    className: checkNextToken 
    getNextToken ;identifier- class name
    getNextToken ;symbol- {
    ClassVarDec 
    SubDec
    getNextToken ;symbol- }
]

;Main
;Tokening
keyWords: ["class" "constructor" "function" "method" "field" "static" "var" "int" "char" "boolean" "void" "true" "false" "null" "this" "let" "do" "if" "else" "while" "return"]
symbols: ["{" "}" "(" ")" "[" "]" "." "," ";" "+" "-" "*" "/" "&" "|" "<" ">" "=" "~"]
numbers: ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9"]
letters: ["a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
           "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"]

path: to-red-file ask "Enter a path to a folder: "
foreach file (read path) [
    if equal? suffix? file ".jack"[
        file-name: copy/part at file 0 ((length? file) - 5)
        output-file: rejoin [path file-name "T.xml"]
        content: read to-red-file rejoin[path file]
        i: 1
        write/append output-file "<tokens>^/"
        while [i < (length? content)] [
            ch: to-string pick content i
            case [
                to-logic find numbers ch [i: IntegerDigit i content output-file]
                ch = "^"" [i: StringChar (i + 1) content output-file]
                (ch = "/" and ((to-string pick content i + 1) = "/")) [i: LineComment (i + 1) content output-file]
                (ch = "/" and ((to-string pick content i + 1) = "*")) [i: LinesComment (i + 1) content output-file] 
                to-logic find symbols ch [i: Symbol i content output-file]
                (ch = "_" or (to-logic find letters ch)) [i: KeyWordIdentifier (i + 1) content output-file] 
                true [i: i + 1]                           
            ]
        ]
        write/append output-file "</tokens>^/"
    ]
]

;Parsing
fieldCounter: 0
staticCounter: 0
varsCounter: 0
argumentCounter: 0
parameterCounter: 0

className: null
allTokens: null
class-scope-symbol-table: []
function-scope-symbol-table: []
i: 2
foreach file (read path) [
    if (copy/part at file ((length? file) - 4) length? file) = "T.xml"[
        file-name: copy/part at file 0 ((length? file) - 5)
        output-file: rejoin [path file-name ".vm"]
        allTokens: read/lines to-red-file rejoin[path file]
        ifCounter: 0
        whileCounter: 0
        openWhile: []
        openIf: []
        i: 2
        ParseClass
    ]
]