" Vim syntax file
" Language:   Decks
" Filenames:  *.decks
" Maintainer: Farbod Salamat-Zadeh


if exists('b:current_syntax')
  finish
endif

let b:current_syntax = 'decks'


syn match decksComment /\v^\/\/.*$/

syn match decksDraw /\v^ *[A-Za-z0-9]+/

syn region decksPropsRegion start=/\[/ end=/\]/ contains=decksHtmlId,decksHtmlCls,decksHtmlStyle
syn match decksHtmlId /\v#[A-Za-z0-9_-]+/ contained
syn match decksHtmlCls /\v\.[A-Za-z0-9_-]+/ contained
syn match decksHtmlStyle /\v\%[a-z-]+/ contained
syn match decksHtmlAttr /\v[a-z-]+/ contained

syn region decksString start=/"/ skip=/\v\\"/ end=/"/
syn region decksStringMulti start=/\[\[/ end=/\]\]/

syn match decksTemplateStr /\v\$[A-Za-z]+\$/
syn match decksKeyword /!def/
syn match decksKeyword /!let/
syn match decksInclude /!include/


hi def link decksComment Comment

hi def link decksDraw Type

hi def link decksHtmlId Identifier
hi def link decksHtmlCls Identifier
hi def link decksHtmlStyle Identifier
hi def link decksHtmlAttr Identifier

hi def link decksString String
hi def link decksStringMulti String

hi def link decksTemplateStr Tag
hi def link decksKeyword Keyword
hi def link decksInclude Include
