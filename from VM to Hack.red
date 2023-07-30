Red []
HandlePushCommand: func [segment [string!] index [integer!] fileS[string!]] [
    switch segment [
        "constant" [ write/append output-file rejoin["@" to-string index "^/D=A^/@SP^/A=M^/M=D^/@SP^/M=M+1^/" ]]
        "local" [ write/append output-file rejoin["@" to-string index "^/D=A^/@LCL^/A=M+D^/D=M^/@SP^/A=M^/M=D^/@SP^/M=M+1^/" ]]
        "argument" [write/append output-file rejoin["@" to-string index "^/D=A^/@ARG^/A=M+D^/D=M^/@SP^/A=M^/M=D^/@SP^/M=M+1^/" ]]
        "this" [write/append output-file rejoin["@" to-string index "^/D=A^/@THIS^/A=M+D^/D=M^/@SP^/A=M^/M=D^/@SP^/M=M+1^/" ]]
        "that" [write/append output-file rejoin["@" to-string index "^/D=A^/@THAT^/A=M+D^/D=M^/@SP^/A=M^/M=D^/@SP^/M=M+1^/" ]]
        "temp" [ write/append output-file rejoin["@" to-string(5 + index) "^/D=M^/@SP^/A=M^/M=D^/@SP^/M=M+1^/" ]]
        "pointer" [ either (index == 0) [write/append output-file "@THIS^/D=M^/@SP^/A=M^/M=D^/@SP^/M=M+1^/"] [write/append output-file "@THAT^/D=M^/@SP^/A=M^/M=D^/@SP^/M=M+1^/"] ]
        "static" [write/append output-file rejoin["@" fileS "." to-string index "^/D=M^/@SP^/A=M^/M=D^/@SP^/M=M+1^/"]]
    ]
]
HandlePopCommand: func [segment [string!] index [integer!] fileS[string!]] [
    switch segment [
        "local" [write/append output-file rejoin["@SP^/A=M-1^/D=M^/@LCL^/A=M^/" ] loop index [write/append output-file "A=A+1^/"] write/append output-file rejoin["M=D^/@SP^/M=M-1^/"] ]
        "argument" [write/append output-file rejoin["@SP^/A=M-1^/D=M^/@ARG^/A=M^/" ] loop index [write/append output-file "A=A+1^/"] write/append output-file rejoin["M=D^/@SP^/M=M-1^/"] ]
        "this" [write/append output-file rejoin["@SP^/A=M-1^/D=M^/@THIS^/A=M^/" ] loop index [write/append output-file "A=A+1^/"] write/append output-file rejoin["M=D^/@SP^/M=M-1^/"] ]
        "that" [write/append output-file rejoin["@SP^/A=M-1^/D=M^/@THAT^/A=M^/" ] loop index [write/append output-file "A=A+1^/"] write/append output-file rejoin["M=D^/@SP^/M=M-1^/"] ]
        "temp" [write/append output-file rejoin["@SP^/A=M-1^/D=M^/@" to-string(index + 5) "^/M=D^/@SP^/M=M-1^/" ]]
        "pointer" [ either (index == 0) [write/append output-file "@SP^/A=M-1^/D=M^/@THIS^/M=D^/@SP^/M=M-1^/"] [write/append output-file "@SP^/A=M-1^/D=M^/@THAT^/M=D^/@SP^/M=M-1^/"] ]
        "static" [write/append output-file rejoin["@SP^/M=M-1^/A=M^/D=M^/@" fileS "." to-string index "^/M=D^/"]]
    ]
]
HandleAddCommand: func[] [
    write/append output-file "@SP^/A=M-1^/D=M^/A=A-1^/M=M+D^/@SP^/M=M-1^/"
]
HandleSubCommand: func[][
    write/append output-file "@SP^/A=M-1^/D=M^/A=A-1^/M=M-D^/@SP^/M=M-1^/"
]
HandleNegCommand: func[][
    write/append output-file "@SP^/A=M-1^/D=M^/M=-D^/"
]
HandleEqCommand: func[][
    write/append output-file rejoin["@SP^/A=M-1^/D=M^/A=A-1^/D=D-M^/@IF_TRUE" to-string counterIf 
    "^/D;JEQ^/D=0^/@SP^/A=M-1^/A=A-1^/M=D^/@IF_FALSE" to-string counterIf
    "^/0;JMP^/(IF_TRUE" to-string counterIf 
    ")^/D=-1^/@SP^/A=M-1^/A=A-1^/M=D^/(IF_FALSE" to-string counterIf
    ")^/@SP^/M=M-1^/"]
    counterIf: counterIf + 1
]
HandleGtCommand: func[][
    write/append output-file rejoin["@SP^/A=M-1^/D=M^/A=A-1^/D=D-M^/@IF_TRUE" to-string counterIf 
    "^/D;JLT^/D=0^/@SP^/A=M-1^/A=A-1^/M=D^/@IF_FALSE" to-string counterIf
    "^/0;JMP^/(IF_TRUE" to-string counterIf 
    ")^/D=-1^/@SP^/A=M-1^/A=A-1^/M=D^/(IF_FALSE" to-string counterIf
    ")^/@SP^/M=M-1^/"]
    counterIf: counterIf + 1
]
HandleLtCommand: func[][
    write/append output-file rejoin["@SP^/A=M-1^/D=M^/A=A-1^/D=D-M^/@IF_TRUE" to-string counterIf 
    "^/D;JGT^/D=0^/@SP^/A=M-1^/A=A-1^/M=D^/@IF_FALSE" to-string counterIf
    "^/0;JMP^/(IF_TRUE" to-string counterIf 
    ")^/D=-1^/@SP^/A=M-1^/A=A-1^/M=D^/(IF_FALSE" to-string counterIf
    ")^/@SP^/M=M-1^/"]
    counterIf: counterIf + 1
]
HandleAndCommand: func[][
    write/append output-file "@SP^/A=M-1^/D=M^/A=A-1^/M=M&D^/@SP^/M=M-1^/"
]
HandleOrCommand: func[][
    write/append output-file "@SP^/A=M-1^/D=M^/A=A-1^/M=M|D^/@SP^/M=M-1^/"
]
HandleNotCommand: func[][
    write/append output-file "@SP^/A=M-1^/D=M^/M=!D^/"
]
HandleLabelCommand: func[segment [string!]][
    write/append output-file rejoin ["(" segment ")^/"]
]
HandleGotoCommand: func[segment [string!]][
    write/append output-file rejoin ["@" segment "^/0;JMP^/"]
]
HandleIfGotoCommand: func[segment [string!]][
    write/append output-file rejoin["@SP^/A=M-1^/D=M^/@SP^/M=M-1^/@" segment "^/D;JNE^/"]
]
HandleFunctionCommand: func[f [string!] k [integer!]][
    write/append output-file rejoin["(" f ")^/"] loop k [write/append output-file "@SP^/A=M^/M=0^/@SP^/M=M+1^/"]
]
HandleCallCommand: func [f [string!] n [integer!]][
    write/append output-file rejoin["@" f ".return-address" counterRA "^/D=A^/@SP^/A=M^/M=D^/@SP^/M=M+1^/"
    "@LCL^/D=M^/@SP^/A=M^/M=D^/@SP^/M=M+1^/"
    "@ARG^/D=M^/@SP^/A=M^/M=D^/@SP^/M=M+1^/"
    "@THIS^/D=M^/@SP^/A=M^/M=D^/@SP^/M=M+1^/"
    "@THAT^/D=M^/@SP^/A=M^/M=D^/@SP^/M=M+1^/"
    "@SP^/D=M^/@" (n + 5) "^/D=D-A^/@ARG^/M=D^/"
    "@SP^/D=M^/@LCL^/M=D^/"]
    HandleGotoCommand f
    HandleLabelCommand rejoin[f ".return-address" counterRA]
    counterRA: counterRA + 1
]
HandleReturnCommand: func[][
    write/append output-file rejoin["@LCL^/D=M^/@13^/M=D^/"
    "@5^/D=D-A^/A=D^/D=M^/@14^/M=D^/"
    "@SP^/A=M-1^/D=M^/@ARG^/A=M^/M=D^/@SP^/M=M-1^/"
    "@ARG^/D=M+1^/@SP^/M=D^/"
    "@13^/M=M-1^/A=M^/D=M^/@THAT^/M=D^/"
    "@13^/M=M-1^/A=M^/D=M^/@THIS^/M=D^/"
    "@13^/M=M-1^/A=M^/D=M^/@ARG^/M=D^/"
    "@13^/M=M-1^/A=M^/D=M^/@LCL^/M=D^/"
    "@14^/A=M^/0;JMP^/"]
]
counterIf: 0
counterVm: 0
counterRA: 0
path: to-red-file ask "Enter a path to a folder: "
last-dir: last split-path path
len: length? last-dir
file-name: copy/part at last-dir 0 (len - 1) 
output-file: rejoin [path file-name ".asm"]
foreach file (read path) [
    if equal? suffix? file ".vm"[
        counterVm: counterVm + 1
    ]
]
if counterVm > 1 [
    write/append output-file "@256^/D=A^/@SP^/M=D^/"
    HandleCallCommand "Sys.init" 0 
]
foreach file (read path) [
    if equal? suffix? file ".vm"[
        len: length? file
        fileS: copy/part at to-string file 0 (len - 3)
        write/append output-file rejoin ["//" fileS]
        write/append output-file "^/"
        foreach line (read/lines to-red-file rejoin[path file]) [
            ln: trim/with line "^-"
            line-words: copy split ln " "
            if not-equal? (pick line-words 1) "//"[
                write/append output-file rejoin["// " line "^/"]
                segment: pick line-words 2
                index: pick line-words 3
                switch pick line-words 1 [
                    "push" [HandlePushCommand segment to-integer index fileS]
                    "pop" [HandlePopCommand segment to-integer index fileS]
                    "add" [HandleAddCommand]
                    "sub" [HandleSubCommand]
                    "neg" [HandleNegCommand]
                    "eq" [HandleEqCommand]
                    "gt" [HandleGtCommand]
                    "lt" [HandleLtCommand]
                    "and" [HandleAndCommand]
                    "or" [HandleOrCommand]
                    "not" [HandleNotCommand]
                    "label" [HandleLabelCommand segment]
                    "goto" [HandleGotoCommand segment]
                    "if-goto" [HandleIfGotoCommand segment] 
                    "function" [f: segment 
                                k: to-integer index
                                HandleFunctionCommand f k]
                    "call" [f: segment
                           n: to-integer index
                           HandleCallCommand f n]
                    "return" [HandleReturnCommand]
                ]
            ]
        ]
    ]
]

