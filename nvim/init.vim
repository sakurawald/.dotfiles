" basic
set number
set filetype=on
set syntax=on
set background=dark
set autoread

" tab line
set showtabline=2
set tabline="%1\ %t%(%m%)%3*"

" command line
set history=10000

" buffer
set wrap
set hidden

" status line
set laststatus=2
set showcmd
set ruler

" mouse
set mouse=

" find and replace
set hlsearch
set incsearch
set ignorecase

" file
set fileencoding="utf-8"

" indent
set autoindent
"set backspace="indent,eol,start"
set expandtab
set smarttab
set shiftwidth=2
set tabstop=2
set softtabstop=2

" leader
map <space> <nop>
let mapleader="\<space>"

" scroll
set sidescroll=1

" split
set splitbelow
set splitright

" complete
set complete=".,w,b,u,t"

" better HJKL
map H ^
map L $
map J }
map K {

" better escape
inoremap jk <Esc>
