" Vim syntax file
" Language:   Decks
" Filenames:  *.decks
" Maintainer: Farbod Salamat-Zadeh


if exists('b:current_syntax')
  finish
endif

let b:current_syntax = 'decks'


syn match decksInvalid /\v^[^\/\W!\-]+/
syn match decksComment /\v^\/\/.*$/

syn match decksDraw /\v^[A-Za-z0-9]+/

syn region decksCssRegion start=/\[/ end=/\]/ contains=decksCssId,decksCssCls,decksCssStyle
syn match decksCssId /\v#[A-Za-z0-9_-]+/ contained
syn match decksCssCls /\v\.[A-Za-z0-9_-]+/ contained
syn match decksCssStyle /\v[a-z-]+\=/ contained

syn region decksContentRegion start=/{/ end=/}/ contains=decksContent,decksTemplate
syn match decksContent /\v[^{}]+/ contains=decksTemplate contained

syn region decksString start=/"/ skip=/\v\\"/ end=/"/

syn match decksTemplate /\v\$[A-Za-z]+\$/ contained
syn match decksKeyword /!def/
syn match decksKeyword /!let/
syn match decksInclude /!include/


hi def link decksInvalid Error
hi def link decksComment Comment

hi def link decksDraw Type

hi def link decksCssId Identifier
hi def link decksCssCls Identifier
hi def link decksCssStyle Identifier

hi def link decksContent String
hi def link decksString String

hi def link decksTemplate Tag
hi def link decksKeyword Keyword
hi def link decksInclude Include
