" Vim filetype plugin file
" Language:   Decks
" Filenames:  *.decks
" Maintainer: Farbod Salamat-Zadeh

if exists('b:did_ftplugin')
  finish
endif
let b:did_ftplugin = 1

let b:undo_ftplugin = 'setlocal formatoptions< comments< commentstring<'

" Set 'formatoptions' to break comment lines but not other lines,
" and insert the comment leader when hitting <CR> or using "o".
setlocal formatoptions-=t formatoptions+=croql

" Set 'comments' to format dashed lists in comments.
setlocal comments=sO:*\ -,mO:*\ \ ,exO:*/,s1:/*,mb:*,ex:*/,://

setlocal commentstring=//%s
