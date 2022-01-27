" --- --- --- --- --- --- --- --- --- ----
" My minimalistic vim config for; 
"  C, and Bash development
" --- --- --- --- --- --- --- --- --- ----

" --- --- --- --- --- --- --- --- --- ----
" Load plugins
" --- --- --- --- --- --- --- --- --- ----
call plug#begin('~/.vim/plugged')
Plug 'valloric/youcompleteme'
Plug 'preservim/nerdtree'
Plug 'mbbill/undotree'
Plug 'morhetz/gruvbox'
call plug#end()
" packadd termdebug

" --- --- --- --- --- --- --- --- --- ----
" Config YCM
" --- --- --- --- --- --- --- --- --- ----
if !exists('g:ycm_semantic_triggers')
	let g:ycm_semantic_triggers = {}
endif

" --- --- --- --- --- --- --- --- --- ----
"   If possible enable termguicolors
" --- --- --- --- --- --- --- --- --- ----
if (has("termguicolors"))
 set termguicolors
endif

" --- --- --- --- --- --- --- --- --- ----
"   Config termdebug (currently disabled)
" --- --- --- --- --- --- --- --- --- ----
set mouse=a
" let g:termdebug_wide=1

" --- --- --- --- --- --- --- --- --- ----
" Other
" --- --- --- --- --- --- --- --- --- ----
set nocompatible
set bg=dark
color gruvbox
colorscheme gruvbox
syntax on
filetype plugin on
" set autochdir
set number relativenumber
set tabstop=4 expandtab shiftwidth=4 smartindent 
set path+=**
set wildmenu 

" ctrl remaps
nnoremap <C-n> :bnext <CR>
nnoremap <C-p> :bprevious <CR>
nnoremap <C-f> :find 
nnoremap <C-t> :NERDTreeToggle <CR>
nnoremap <C-u> :UndotreeToggle <CR>

" space remaps
let mapleader = " "
nnoremap <leader>g :YcmCompleter GoToDefinition <CR>
nnoremap <leader>G :YcmCompleter GoToReferences <CR>
" nnoremap <leader>d :Termdebug <CR> 
    " | hi debugPC term=reverse ctermbg=4 guibg=darkblue
    " | hi debugBreakpoint term=reverse ctermbg=red guibg=red

" C
" autocmd FileType c nnoremap <leader>d :!gdb $(basename % .c) <CR>
autocmd FileType c command Debug :!gdb $(basename % .c) --eval-command="set style enable off" --eval-command="set logging on" --eval-command="layout src"<CR>
