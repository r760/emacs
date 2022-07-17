" --- --- --- --- --- --- --- --- --- ----
" My minimalist vim config for;
"  C, and Bash development
" --- --- --- --- --- --- --- --- --- ----

" --- --- --- --- --- --- --- --- --- ----
" Load plugins
"
" ycm installation details
"   -> https://vimawesome.com/plugin/youcompleteme
" --- --- --- --- --- --- --- --- --- ----
call plug#begin('~/.vim/plugged')
Plug 'valloric/youcompleteme'
Plug 'preservim/nerdtree'
Plug 'mbbill/undotree'
Plug 'lifepillar/vim-solarized8'
call plug#end()

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
" Other
" --- --- --- --- --- --- --- --- --- ----
syntax on
filetype plugin on

color solarized8_flat
colorscheme solarized8_flat

set nocompatible
set relativenumber
set tabstop=4
set expandtab
set shiftwidth=4
set smartindent
set path+=**
set wildmenu
set mouse=a

" ctrl remaps
nnoremap <C-j> }
nnoremap <C-k> {
nnoremap <C-n> :bnext <CR>
nnoremap <C-p> :bprevious <CR>
nnoremap <C-o> :only <CR>
nnoremap <C-f> :find
nnoremap <C-t> :NERDTreeToggle <CR>
nnoremap <C-u> :UndotreeToggle <CR>

" space remaps
let mapleader = " "
nnoremap <leader>f :YcmCompleter GetDoc <CR>
nnoremap <leader>g :YcmCompleter GoToDefinition <CR>
nnoremap <leader>G :YcmCompleter GoToReferences <CR>

" C
autocmd FileType c command! Make  :!clear; make $(basename % .c)
autocmd FileType c command! Clean :!clear; make clean
autocmd FileType c command! Debug :!clear; gdb $(basename % .c) --eval-command="set style enable off" --eval-command="layout src"
