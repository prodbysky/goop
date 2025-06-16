if exists("b:current_syntax")
  finish
endif

" Keywords
syntax keyword goopKeyword return let true false if while

" Operators
syntax match goopOperator "[+\-*/<>]%"

" Delimiters
syntax match goopParen "[()]"
syntax match goopCurly "[{}]"
syntax match goopSemicolon ";"
syntax match goopColon ":"
syntax match goopAssign "="

" Identifiers
syntax match goopIdentifier "\<[A-Za-z_][A-Za-z0-9_]*\>"

" Numbers
syntax match goopNumber "\<\d\+\>"

" Errors
syntax match goopError "[^ \t\n\r0-9A-Za-z_{}()[\];:+\-*/=<>]"

" Define highlight groups
highlight default link goopKeyword Keyword
highlight default link goopOperator Operator
highlight default link goopParen Delimiter
highlight default link goopCurly Delimiter
highlight default link goopSemicolon Delimiter
highlight default link goopColon Delimiter
highlight default link goopAssign Operator
highlight default link goopIdentifier Identifier
highlight default link goopNumber Number

let b:current_syntax = "goop"


