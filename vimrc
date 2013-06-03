" Vundle setup
filetype off
set rtp+=~/.vim/bundle/vundle
call vundle#rc()

Bundle 'gmarik/vundle'
Bundle 'lordm/vim-browser-reload-linux'

" workflow
Bundle 'jceb/vim-orgmode'
Bundle 'goldfeld/vimdow'
Bundle 'mikewest/vimroom'
Bundle 'goldfeld/vim-micro'

" writing
Bundle 'goldfeld/tnt'

let g:surround_no_mappings = 1
let g:surround_no_insert_mappings = 1
nnoremap d`  <Plug>Dsurround
nnoremap c`  <Plug>Csurround
nnoremap y`  <Plug>Ysurround
nnoremap y~  <Plug>YSurround
nnoremap y`` <Plug>Yssurround
nnoremap y~` <Plug>YSsurround
nnoremap r~~ <Plug>YSsurround
xnoremap ~   <Plug>VSurround
xnoremap g~  <Plug>VgSurround
inoremap <C-S>   <Plug>Isurround
inoremap <C-G>` <Plug>Isurround
inoremap <C-G>~ <Plug>ISurround

" editing
Bundle 'tpope/vim-surround'

" files
Bundle 'vim-scripts/grep.vim'
Bundle 'tpope/vim-fugitive'
"Bundle 'spolu/dwm.vim'

map <silent> 'e <Plug>CamelCaseMotion_e
map <silent> 'b <Plug>CamelCaseMotion_b
map <silent> 'w <Plug>CamelCaseMotion_w
" moving
Bundle 'bkad/CamelCaseMotion'
Bundle 'goldfeld/vim-seek'

" syntax
Bundle 'kchmck/vim-coffee-script'
Bundle 'jb55/Vim-Roy'
Bundle 'leafo/moonscript-vim'
Bundle 'vim-scripts/Vim-R-plugin'
Bundle 'goldfeld/criticmarkup-vim'
Bundle 'wting/rust.vim'

" other
Bundle 'goldfeld/vim-pegword'

" colorschemes
Bundle 'croaker/mustang-vim'
Bundle 'morhetz/gruvbox'
Bundle 'jonathanfilip/vim-lucius'
Bundle 'vim-scripts/candycode.vim'
Bundle 'rainerborene/vim-heroku'

filetype plugin indent on
set runtimepath^=~/.vim/bundle/ctrlp.vim
color slate
set cursorline
let &colorcolumn=join(range(81, 201), ",")

set tabstop=2                   " number of spaces of tab character
set shiftwidth=2                " number of spaces to (auto)indent
set smarttab                    " put tabs on BOL as per shiftwidth, not tabstop
set expandtab
set smartindent
set autoindent
set fillchars=vert:\ ,fold:\ 

set backspace=indent,eol,start  " allow <C-H> over everything in insert mode
set scrolloff=3                 " keep 3 lines when scrolling
set hlsearch                    " hightlight searches
set incsearch                   " do incremental searching
set ignorecase                 
set smartcase                   " ignore case of search only if all lowercase

set hidden                      " buffer switch w/o saving; keeps undo history
set autoread                    " reload changed files if there's no conflict
set autochdir                   " keep working dir relative to current file
set viminfo+=!
set previewheight=20            " height of the preview window

"set wildmenu
"set wildmode="full"
"set wildcharm=<C-Z>
set wildignorecase

if filereadable(expand("~/punchcard"))
  autocmd BufRead,BufNewFile *.coffee,*.js,*.html,*.css setlocal noexpandtab
  set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.user,*.nupkg,*.dll,*.xml,
    \*.config,*.suo,*.sln,*.asax,*.cs,*.transform,*.ttf,*.ico,*._,*.c,*.h,*.mk,
    \*.js,*/build/*
else
  set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.user,*.nupkg,*.dll,*.xml,
    \*.config,*.suo,*.sln,*.asax,*.cs,*.transform,*.ttf,*.ico,*._,*.c,*.h,*.mk
endif

" rust's conceal (replacing stuff with unicode) doesn't work in gvim
let g:no_rust_conceal = 1

augroup filetypeSettings
  autocmd!
  autocmd BufEnter * call FtColors()
  autocmd BufRead,BufNewFile *.vim setlocal foldmethod=marker
  autocmd BufRead,BufNewFile *.rs setlocal shiftwidth=4 tabstop=4
  autocmd BufRead,BufNewFile *.md setlocal colorcolumn=0
  autocmd BufRead,BufNewFile *.tnt.* setlocal expandtab
augroup END

function! FtColors()
  if match(['rust'], &ft) != -1
    if g:colors_name != 'heroku' | color heroku | endif
  elseif &diff || match(['vim', 'perl', 'html', 'diff'], &ft) != -1
    if g:colors_name != 'gruvbox' | color gruvbox | endif
  else | if g:colors_name != 'mustang' | color mustang | endif
  endif
  highlight ColorColumn guibg=#373737 ctermbg=236 |
  highlight CursorLine guibg=#373737 ctermbg=236 |
  highlight CursorColumn guibg=#373737 ctermbg=236
endfunction

set laststatus=2
set statusline=
set statusline+=%{fugitive#statusline()}
set statusline+=\ %f 
set statusline+=\ %m
set statusline+=%h
set statusline+=%{GetModifiedBuffers()}

function! GetModifiedBuffers()
  redir @b
  silent! buffers
  redir END
  return system('echo "'.@b.'"' . " | awk '$3 ~ /\\+/ {printf ".'"  " $4 "*"'."}'")
endfunction

" gvim behave like vim: console tabs and no dialogs, menus or scrollbars
set guioptions+=lrbmTLce
set guioptions-=lrbmTLce
set guioptions+=c
" modelines can execute arbitrary code in e.g. set foldexpr
set modelines=0

let mapleader = ","

let g:TNTWebBrowser = 'luakit'

let g:Vimdow = {}
augroup DOW
  autocmd BufRead,BufNewFile *.dow call ReadDow()
   autocmd VimEnter * nested if argc() == 0 && filereadable(".dow") |
     \ call ReadDowFile(".dow")
augroup END

function! ReadDowFile(path)
  let lines = readfile(a:path)
  let bufopen = []

  for line in l:lines
    let char = line[0]
    if l:char == '$'
      let exeline = line[1:]
      call system(line[1:])
      if stridx(line, "grunt") != -1
        let escline = substitute(line[1:], '"', '\\"', 'g')
        execute 'nnoremap <Leader>o :call system("' l:escline '"' ")\<CR>"
      endif
    elseif l:char == '.' || l:char == '/'
      call add(bufopen, line)
    endif
  endfor

  for fname in l:bufopen
    execute "badd" fname
  endfor
endfunction

function! FindGitPrj(...)
  let returnoptions = a:0 ? split(a:1, ',') : ['folder']
  let originalpath = a:0 >= 2 ? a:2 : expand('%:p:h')
  let dirs = split(l:originalpath, '/')
  let path = ''
  for idx in range(len(l:dirs))
    let dir = l:dirs[idx]
    let l:path = l:path . '/' . l:dir

    if isdirectory(l:path . '/.git/')
      let returnpaths = []
      for returnoption in returnoptions
        if returnoption ==? 'absolute' | call add(returnpaths, l:path)
        elseif returnoption ==? 'relative'
          call add(returnpaths, join(l:dirs[idx : -1], '/'))
        else | call add(returnpaths, l:dir)
        endif
      endfor

      if len(returnoptions) == 1 | return returnpaths[0]
      else | return returnpaths
      endif

    endif
  endfor

  let l:originalpath = fnamemodify(l:originalpath, ':~')
  let returnpaths = []
  for returnoption in returnoptions
    if returnoption ==? 'absolute' | call add(returnpaths, l:originalpath)
    elseif returnoption ==? 'relative' | call add(returnpaths, l:originalpath)
    else | call add(returnpaths, '')
    endif
  endfor
  if len(returnoptions) == 1 | return returnpaths[0]
  else | return returnpaths
endfunction

nnoremap <Leader>c :Vimdow Chrome<CR>
nnoremap <Leader>h :Vimdow http<CR>:Vimdow Luakit<CR>
" working terminal
nnoremap <Leader>B :Vimdow fish<CR>:Vimdow @vitoria<CR>
nnoremap <Leader>b :execute "Vimdow " . FindGitPrj()<CR>
" server task
nnoremap <Leader>u :Vimdow sudo<CR>:Vimdow meteor<CR>
" compiler task
if get(g:Vimdow, 'grunt', 1)
  nnoremap <Leader>o :Vimdow coffee<CR>:Vimdow grunt<CR>:Vimdow compass<CR>
endif

nnoremap <Leader>C :Vimdow Chrome o<CR>
nnoremap <Leader>H :Vimdow Luakit o<CR>
nnoremap <Leader>S :Vimdow fish o<CR>
nnoremap <Leader>O :Vimdow coffee o<CR>
nnoremap <Leader>M :Vimdow meteor o<CR>
nnoremap <Leader>G :Vimdow gedit o<CR>

" skip past big lines
nnoremap gj j
nnoremap gk k

" go to last inserted 
nnoremap gi '^

nnoremap <silent> <Space><Space> @=(foldlevel('.')?'za':"\<Space>")<CR>
nnoremap 'm :TNTVisibleHeadingNext<CR>

nnoremap Y y$
nnoremap yY ggyG
nnoremap dD ggdG

" insert two en-dashes (&#8211)
inoremap <C-D> ––
" save my pinky
inoremap <Backspace> <NOP>
nnoremap ZZ <NOP>

" temporary crutches
nmap O <NOP>
autocmd BufRead *.tnt.* nnoremap "* :echo "use ^R"<CR>

nnoremap <silent> j
  \ :<C-U>call RestrainCommand('j', "normal! }", v:count1, 2)<CR>
nnoremap <silent> k
  \ :<C-U>call RestrainCommand('k', "normal! {", v:count1, 2)<CR>

" move a line of text using ALT-{j,k}
" bind these to jk and kj (restrained)
nnoremap mj mz:m+<CR>`z
nnoremap mk mz:m-2<CR>`z

nnoremap <silent> h :<C-U>call RestrainCommand('h', "", v:count1)<CR>
nnoremap <silent> l :<C-U>call RestrainCommand('l', "", v:count1)<CR>

"nnoremap <silent> - :<C-U>call RestrainCommand('$', "", v:count)<CR>
" map minus to do a cut short seek in operator pending mode.
let g:SeekCutShortKey = '-'

"nnoremap <silent> _ :<C-U>call RestrainCommand('^', "", v:count)<CR>
" map underscore to do a cut short back seek in operator pending mode.
let g:SeekBackCutShortKey = '_'

nnoremap _ gE
" e works as default , double e should do a end-of-word seek, '-e' should do '^'
nnoremap <silent> e :<C-U>call RestrainCommandPair('e', 'e', "echo 'ee'", '-',
  \ 'normal! ^')<CR>
" - works as 'ge', double - should do a start-of-word seek, 'e-' should do '$'
nnoremap <silent> - :<C-U>call RestrainCommandPair('ge', '-', "echo '--'", 'e',
  \ 'normal! $')<CR>

" step back one char so it doesn't include the newline character.
vnoremap - $h

" TODO map double ^ to do begin of line seek
" TODO map double $ to do end of line seek

augroup restrainCommand
  autocmd!
  autocmd CursorMoved * call CheckCurrentCommand()
augroup END

let g:currentCommand = ''
let g:lastCommand = ''
function! RestrainCommand(cmd, doublePressCmd, ...)
  " the two splat arguments we might get are first a
  " count to pass to the restrained command, and maybe
  " an integer saying at what minimum count we should
  " override the restrainment, that is, ignore it.
  " a:0 represents how many optional arguments we got.
  if a:0 >= 1 | let cnt = a:1
  else | let cnt = ''
  endif

  " if an override was passed and the passed count meets
  " the override value, we set our boolean to that value,
  " which means simply setting it to true.
  if a:0 >= 2 && l:cnt >= a:2 | let countOverride = a:2
  else | let countOverride = 0
  endif

  if g:lastCommand == a:cmd && !l:countOverride
    execute a:doublePressCmd
  else | execute 'normal! ' . l:cnt . a:cmd
  endif
  let g:currentCommand = a:cmd
endfunction

function! RestrainCommandPair(cmd, key, doubleCmd, pair, pairThenCmd)
  if g:lastCommand == a:key | execute a:doubleCmd
  elseif g:lastCommand == a:pair | execute a:pairThenCmd
  else | execute "normal!" a:cmd
  endif
  let g:currentCommand = a:key
endfunction

function! CheckCurrentCommand()
  let g:lastCommand = ''
  if g:currentCommand != ''
    let g:lastCommand = g:currentCommand
  endif
  let g:currentCommand = ''
endfunction

"inoremap <C-I> <C-O>:WriterMode<CR>

function! WriterTimestamp()
  let date = system('date +%s%N | cut -b1-13')
  return strpart(l:date, 0, len(l:date) - 1)
endfunction
command! -nargs=0 WriterMode call WriterMode()
function! WriterMode()
  let g:writerModeStart = WriterTimestamp()
  let esc = maparg('<Esc>')
  let bksp = maparg('<Backspace>')
  if l:esc
    let g:writerModeStoreEsc = l:esc
    unmap <Esc>
  endif
  if l:bksp
    let g:writerModeStoreBksp = l:bksp
    unmap <Backspace>
  endif
  inoremap <silent> <Esc> <Esc>:WriterModeEnd<CR>
  inoremap <silent> <Backspace> <NOP>
  startinsert
endfunction
command! -nargs=0 WriterModeEnd call WriterModeEnd()
function! WriterModeEnd()
  let g:writerModeEnd = WriterTimestamp()
  echo 'hey'
  unmap <Esc>
  unmap <Backspace>
  if g:writerModeStoreEsc | execute 'nnoremap | endif
  "echo 'Writing started at '.g:writerModeStart.', '.g:writerModeEnd.' elapsed; X words typed, 180wpm'
endfunction

nnoremap <Backspace> :call Backspace()<CR>
function! Backspace()
  " need to check the current word
  " if we're at the end of it, need to 
  " leave us at the end of the previous.

  " also remap <Enter> to 'ea' or 'eal'

  " get current column
  let cursor = getpos('.')[2]
  " get line text
  let line = getline('.')
  " if we're not at the beginning of the word, go to it.
  if !(l:line[l:cursor - 2] =~ '\s')
    execute "normal! b"
  endif

  let newcursor = getpos('.')[2]
  let delta = l:cursor - l:newcursor
  execute "normal! dawb"

  let max = len(expand("<cword>")) - 1
  let move = 0
  if l:max > l:delta
    let l:move = l:delta
  else
    let l:move = l:max
  endif
  
  execute "normal! ".l:move."l"
endfunction

" sacrifice some marks for my pinkies' sake
nnoremap mm :
nnoremap mw :w<CR>
nnoremap mq :q<CR>
nnoremap mx :x<CR>
nnoremap mv :vs<CR>
nnoremap mz :sp<CR>
nnoremap mh :help 

" other marks for my other pinky
nnoremap mo O
nnoremap ma A
nnoremap m. zz
nnoremap m, zb
nnoremap m' zt

" search for standalone word (no substring matches)
nnoremap '/ /\<\><Left><Left>

nnoremap <silent> mp :call PurgeBuffers()<CR>
function! PurgeBuffers()
  let seq = ''
  buffers
  echo "Delete buffers by number: "
  let char = getchar()

  while l:char != 27 && l:char != 3
    " if a number was entered, append it to our sequence.
    if l:char >= 48 && l:char <= 57
      let l:seq = l:seq . nr2char(l:char)
    endif

    " if we've already got two chars entered, or user explicitly presses enter.
    if len(l:seq) > 1 || l:char == 13
      execute "bdelete " l:seq
      let l:seq = ''
      redraw
      buffers
      echo "Delete buffers by number: "
    endif

    let l:char = getchar()
  endwhile
  redraw
endfunction

" b mark for closing buffer.
nnoremap <silent> mb :call BufAway("keepalt edit", "bufaway", 0, {
  \ 'query': GetLocalBufList() })<CR>

function! GetLocalBufList(...)
  let execute = a:0 && a:1 == 1
  " can be simplified using getftime(); then sort() the array of dicts
  " also look into shellescape()

  let b1  = "find . -name .\\*.swp | " " find all swap files
  let b2  = "tee buflst            | " " save a temporary copy of the filenames
  let b3  = "xargs -l stat         | " " get file stats (-l means pipe linewise)
  let b4  = "(awk -F '[-:. ]'        " " awk will get the file's last access
  let b5  = "'/^Access: 2/ { print $3 $4 $5 $6 $7 $8; }'; "
  let b6  = "awk '{print}' buflst) | " " a 2nd awk retrieves filename temp copy
  let b7  = "xargs -0 -n 2         | " " pipe results 2 by 2 (one for each awk)
  let b8  = "awk 'BEGIN {c=0}        " " stitch the two args together, then sort
  let b9  = "/^[0-9]+$/ {arr[++c]=$0} "
  let b10 = "/swp$/ { print arr[NR - c] \" \" $0 }' | sort | "
  let b11 = "awk '{ sub(/\\.swp/, \"\"); sub(/\\.\\/\\.?/,\"\"); "
    \. "sub(/\\/\\./,\"/\"); print }'"
  let b12 = " | awk '{a=\"\"; for (i=2; i<=NF; i++) { a=a \" \" $i }; print a }'"

  let query = b1.b2.b3.b4.b5.b6.b7.b8.b9.b10.b11.b12
  if l:execute | return split(system(l:query), "\n") | endif
  return l:query
endfunction

function! GetBufList(...)
  let ids = filter(range(1, bufnr('$')), 'empty(getbufvar(v:val, "&bt"))'
    \ . ' && getbufvar(v:val, "&bl") && strlen(bufname(v:val))')
  if !a:0 | return map(ids, 'fnamemodify(bufname(v:val), ":p")')
  elseif a:1 == 'id' | return ids

  else
    let buflist= []
    let currentgit = FindGitPrj()
    if a:1 == 'ls' | for id in ids
      call add(l:buflist, id . ':' .
        \ FindGitPrj('relative', fnamemodify(bufname(id), ':p')) )
    endfor
    elseif a:1 == 'local' | for id in ids
      let git = FindGitPrj('relative,folder', fnamemodify(bufname(id), ':p'))
      " skip this id if the buffer's git folder isn't our current git folder.
      if l:currentgit !=# l:git[1] | continue | endif
      call add(l:buflist, id . ':' . l:git[0])
    endfor | endif
    return l:buflist
  endif
endfunction

nnoremap <Leader><Leader> <C-^>

" repurpose the colon as my comma lost to leader.
nnoremap : ,

" visual shifting (relect after shift).
vnoremap < <gv
vnoremap > >gv

" save file opened without sudo after the fact
cmap W w !sudo tee % >/dev/null

noremap <silent> K :execute "normal i".nr2char(getchar())<CR>

" paste from clipboard
"set clipboard=unnamed

nnoremap <Leader>[ {o
nnoremap <Leader>] }O

" to go end of textwidth.
nnoremap <Leader>- 81\|
" pull next line and delete any comment symbols.
nnoremap <Leader>J Jldw
" delete to first char in line, including current char.
nnoremap <Leader>D d^x

" go to the function name from within a function
let expr = '\(^fun\S* \)\@<=[^f][^u][^n]\w\+\<Bar>^\w\+'
execute "nnoremap <Leader>f ?".expr."<CR>"

nnoremap <Leader>v :call LoadSession()<CR>

" toggle show hidden characters and cursorcolumn
nnoremap <Leader>l :set list!<CR>:set cursorcolumn!<CR>
" useful for uncommenting lines
nnoremap <Leader>i _wi
" output current time and date with year and week, all pretty printed.
nnoremap <silent> <Leader>d :execute "echo system(\"date +'[%Yw%W] %b %-e %a <%H:%M>'\")"<CR>

let g:MicroMarks = ['h', 't', 'n', 's', 'l', 'r' ]
nnoremap mi :MicroMark<CR>
nnoremap md :MicroMarkClear<CR>
nnoremap 'c :MicroMarkMatch<CR>
for micromark in g:MicroMarks
  execute "nnoremap '" . micromark . " `" . micromark . "zvzz"
endfor

nnoremap <Leader>t :call EditOtherExt('.coffee')<CR>
nnoremap <Leader>n :call EditOtherExt('.html')<CR>
nnoremap <Leader>s :call EditOtherExt('.scss')<CR>
function! EditOtherExt(ext)
  let filename = expand('%')
  execute 'e ' split(filename, '\.')[0] . a:ext
endfunction

command! -nargs=0 Sum :5,12!awk '{num = substr($7, 2, length($7) - 4) + substr($8, 2, length($7) - 4); width += num; print} END {print width}'

" common searches
nnoremap <Leader>/h /HEAD<CR>
nnoremap <Leader>/c /console<CR>

" convert time since epoch under cursor to readable time.
nnoremap <Leader>.m :echo FromEpoch(expand("<cword>"))<CR>
function! FromEpoch(date)
  let date = strpart(a:date, 0, len(a:date) - 3)
  return system('date --date "Jan 1, 1970 00:00:00 +000 + '.l:date.' seconds"')
endfunction

" quickly edit my tnt outline
nnoremap <silent> <Leader>.t :e ~/goldfeld/.tnt/lifethreads.tnt.md<CR>
" allow left ctrl (which I remap to my Caps Lock key) to act as <Esc> when pressed alone.
nnoremap <silent> <Leader>.x :execute "call system(\"~/./xcape -e 'Control_L=Escape'\")"<CR>
" grab ssh publickey to clipboard.
nnoremap <silent> <Leader>.k :execute "call system(\"xclip -sel clip < ~/.ssh/id_rsa.pub\")"<CR>

" remove swap files for current file.
nnoremap <silent> <Leader>.r :silent! exe "!rm ." .expand('%'). ".sw*"<CR>

" quickly edit my vimrc.
nnoremap <silent> <Leader>.v :e ~/goldfeld/dotfiles/vimrc<CR>
" source vimrc to allow live reloading of changes.
nnoremap <silent> <Leader>.V :w<CR>:so $MYVIMRC<CR>:color gruvbox<CR>
" quickly edit a vim bundle
nnoremap <silent> <Leader>.b :call Dmenu("edit", "bundle", {
  \ 'query': 'ls $HOME/.vim/bundle/', 'prepend': '$HOME/.vim/bundle/',
  \ 'append': "/README.md" })<CR>

nnoremap <silent> <Esc> :noh<CR><Esc>
" toggle uppercase/lowercase.
inoremap <C-B>t <Esc>vb~gvova
" same as above but going over underscores.
inoremap <C-B><C-T> <Esc>vB~gvova
" toggle uppercase/lowercase of whole line.
inoremap <C-B><C-L> <Esc>v^~gvova
" toggle case of first letter.
inoremap <C-B><C-P> <Esc>bv~ea

" make C-U and C-W undoable by using <C-G>u (signal a new change to vim.)
inoremap <C-U> <C-G>u<C-U>
inoremap <C-W> <C-G>u<C-W>

" use register 'u' for emulating terminal C-U & C-Y on vim command line.
cnoremap <C-U> <C-\>e(setreg("u", getcmdline())?"":"")<CR>
cnoremap <C-Y> <C-R>u

" operator-pending
onoremap in( :<C-U>normal! f(vi(<CR>
onoremap in< :<C-U>normal! f<vi<<CR>
onoremap in[ :<C-U>normal! f[vi[<CR>
onoremap ih :<C-U>execute "normal! ?^==\\+$\r:noh\rkvg_"<CR>
onoremap ah :<C-U>execute "normal! ?^==\\+$\r:noh\rg_vk0"<CR>

onoremap aa :<C-U>call AttrTextObj()<CR>

function! AttrTextObj()
  let res = searchpair("\['\"\]", "", "\['\"\]")
  if l:res == 0 || l:res == -1
    normal! f=
    let res = searchpair("\['\"\]", "", "\['\"\]")
    echo res
  endif
  execute 'normal! v' 
endfunction

inoremap <A-C> <A-U>

nnoremap <C-;> yl:execute "normal! f" . @"<CR>
nnoremap <C-:> yl:execute "normal! F" . @"<CR>

"let g:ctrlp_extensions = ['commitdriven']
"noremap <C-T> :CommitDriven<CR>
":noremap <Leader><Leader> :CommitDrivenLeader<CR>

let g:ctrlp_user_command =
  \ ['.git', 'cd %s && git ls-files . --cached --exclude-standard --others']
let g:ctrlp_prompt_mappings = {
  \ 'PrtBS()': ['<c-h>', '<c-]>'],
  \ 'PrtCurLeft()': ['<left>'],
  \ }

function! NemoMaps()
  let nemobuf = [
    \ ['<C-P>', 'BufAway', 'edit', 'prjaway', "8"],
    \ ['<C-N>', 'Dmenu',   'edit', 'prjopen', "8"],
    \ ['<C-T>', 'Dmenu', 'buffer', 'bufcycle', "0, { 'farray':
      \ GetBufList('ls'), 'process': 'split(v:val, \":\")[0]' }"],
    \ ['<C-B>', 'BufAway', 'buffer', 'bufaway', "0, { 'farray':
      \ GetBufList('ls'), 'process': 'split(v:val, \":\")[0]' }"],
    \ ['<C-C>', 'Dmenu', 'buffer', 'loccycle', "0, { 'farray':
      \ GetBufList('local'), 'process': 'split(v:val, \":\")[0]' }"],
    \ ['<C-R>', 'BufAway', 'buffer', 'locaway', "0, { 'farray':
      \ GetBufList('local'), 'process': 'split(v:val, \":\")[0]' }"]
    \]

  for c in l:nemobuf
    execute "nnoremap <silent> <C-T>" . l:c[0] ":call" l:c[1] . "('"
      \ l:c[2] "','" . l:c[3] . "'," l:c[4] . ")\<CR>"
  endfor
endfunction
call NemoMaps()

command! -nargs=1 -complete=file E execute "edit +bdelete\\" bufnr('%') <f-args>

function! BufAway(cmd, prompt, ...)
  if a:0 && a:1 | let list = a:1 | else | let list = 0 | endif
  if a:0 >= 2 | let opts = a:2 | else | let opts = { } | endif

  let buf = bufnr('%')
  let result = Dmenu('keepalt ' . a:cmd, a:prompt, l:list, l:opts)
  if l:result | execute "bdelete" l:buf | endif
endfunction

" strip the newline from the end of a string
function! Chomp(str)
  return substitute(a:str, '\n$', '', '')
endfunction

" find a file and pass it to cmd; credit: leafo (initial idea and code)
function! Dmenu(cmd, prompt, ...)
  if a:0 && a:1 | let list = ' -l '.a:1.' ' | else | let list = '' | endif
  if a:0 >= 2 | let opts = a:2 | else | let opts = { } | endif

  let fnames = get(l:opts, 'farray', [])
  let process = get(l:opts, 'process', 'v:val')
  if !empty(l:fnames) | let q = 'printf %"s\n" ' . join(l:fnames, " ")
  else | let q = get(l:opts, 'query', 'git ls-files '.FindGitPrj('absolute'))
  endif

  let prepend = get(l:opts, 'prepend', '')
  let append = get(l:opts, 'append', '')

  let choice = Chomp(system(l:q." | dmenu -i -p " . a:prompt . l:list))
  if empty(l:choice) | return 0 | endif
  execute a:cmd l:prepend . map([l:choice], l:process)[0] . l:append
  return 1
endfunction

let g:seek_enable_jumps = 1
let g:seek_use_vanilla_binds_in_diffmode = 1
let g:seek_char_aliases =
  \ "[{ ]} 9( 8* 7& 6^ 5% 4$ 3# 2@ 1! 0) \| ;: ,< .> `~ -_ /? =+ '" . '"'

nnoremap <silent> <Leader>A :call Sass()<CR>
function! Sass()
  " get current column
  let cursor = getpos('.')[2]
  " get line text
  let line = getline('.')

  let beginning = ''
  let word = expand("<cWORD>")
  if stridx(l:word, '(') != -1
    " if we're not at the beginning of the sass invocation, go to it.
    if !(l:line[l:cursor - 2] =~ '\s')
      let l:beginning = 'B'
    endif
  else
    let l:beginning = 'F(B'
  endif

  execute 'normal! ' . l:beginning . 'vf)"qy'
  let output = system('echo "' . @q . '" | sass -i | xargs echo')
  let @q = strpart(l:output, matchend(l:output, @q) + 1, 7)
  execute 'normal! gv"qp'
endfunction

" can't map <C-I> to anything else since it's the same as <Tab>.
nnoremap <Tab> :CtrlPBuffer<CR>

" give up single d visual delete so I can emulate a diff buffer's normal
" mode mappings in visual mode too with linewise control.
vnoremap <silent> dd :delete<CR>
vnoremap <silent> do :diffget<CR>
vnoremap <silent> dp :diffput<CR>

nnoremap qf :cwindow<CR>
"nnoremap qg :let b:qfbufs = cfirst<CR>
nnoremap <silent> qg @=(&diff?"gg]c":":cfirst\r")<CR>
nnoremap <silent> qc @=(&diff?"]c":":cn\r")<CR>
nnoremap <silent> qr @=(&diff?"[c":":cN\r")<CR>
nnoremap <silent> ql @=(&diff?":diffupdate\r":":call CloseQFBufs()\r")<CR>
" double q to make sure recording stops.
nnoremap qq q

function! CloseQFBufs()
  " map quickfix dicts to bufnr's, then filter out non-open (listed) buffers.
  let bufs = filter(map(getqflist(), 'v:val.bufnr'), 'getbufvar(v:val, "&bl")')
  let chosenbuf = bufnr('%')
  for listedbuf in l:bufs
    " don't close the quickfix buffer if it's the one we're on.
    if listedbuf != l:chosenbuf | silent! execute "bdelete" listedbuf | endif
  endfor
endfunction

nnoremap gs :call Gcached()<CR>
function! Gcached()
  let cached = split(system('git diff --cached'), "\n")
  let changed = []
  let lastWasDiff = 0

  for line in l:cached
    if line =~# '\v^(\+[^+]|\-[^-])'
      if !lastWasDiff | call add(l:changed, "") | endif
      call add(l:changed, line)
      let lastWasDiff = 1
    elseif line =~# '\v^(\+|\-)'
      call add(l:changed, line)
    else | let lastWasDiff = 0
    endif
  endfor

  "let l:changed = filter(l:cached, 'v:val[0] =~# "[+-]"')
  if empty(l:changed) | Gstatus
  else
    let g:changedtemp = resolve(tempname())
    call writefile(l:changed, g:changedtemp)
    silent execute
      \ "pedit +setlocal\\ bt=nowrite\\ ft=diff\\ nomodified" g:changedtemp
    wincmd P
    nnoremap <buffer> <silent> C :wincmd p<CR>:pclose<CR>
      \:sleep 100m<CR><C-U>:Gcommit<CR>:exe "vsplit" g:changedtemp<CR><C-W>li
  endif
endfunction

" vim-fugitive plugin mappings
nnoremap gb :Gblame<CR>
nnoremap gB :Gbrowse<CR>
nnoremap gl :Glog<CR>
nnoremap gn :Ggrep! "<cword>"<CR>
vnoremap gn y:Ggrep! <C-R>"<CR>

" same as git add the current file.
nnoremap gt :Gwrite<CR>
" same as git checkout the current file, updating buffer.
nnoremap gx :Gread<CR>
" leave me on the index version, so I can quickly check it and close it.
nnoremap gc :Gdiff<CR><C-W>h
" use 'help index' to see vim's built-in natively mapped keys

nnoremap <Leader>* :set hls<CR>:AutoHighlightToggle<CR>
command! -nargs=0 AutoHighlightToggle call AutoHighlightToggle()
function! AutoHighlightToggle()
  let @/ = ''
  if exists('#auto_highlight')
    au! auto_highlight
    augroup! auto_highlight
    setl updatetime=4000
    echo 'Highlight current word: off'
    return 0
  else
    augroup auto_highlight
      au!
      au CursorHold * let @/ = '\V\<'.escape(expand('<cword>'), '\').'\>'
    augroup end
    setl updatetime=500
    echo 'Highlight current word: ON'
    return 1
  endif
endfunction

":noremap <Space> :LineSeekToggle<CR>
command! -nargs=0 LineSeekToggle call LineSeekToggle()
let LineSeek = 0
let seq = '1234567890'
let qes = '!@#$%^&*()'
let seqlen = len(seq)
function! LineSeekToggle()
  if g:LineSeek == 0
    let g:LineSeek = 1
    let i = 0
    while i < g:seqlen
      execute 'nmap '.g:seq[i].' :<C-U>LineSeek '.g:seq[i].'<CR>'
      execute 'nmap '.g:qes[i].' :<C-U>LineSeekBack '.g:qes[i].'<CR>'
      let i = i + 1
    endwhile
  else
    let g:LineSeek = 0
    let i = 0
    while i < g:seqlen
      execute 'unmap '.g:seq[i]
      execute 'unmap '.g:qes[i]
      let i = i + 1
    endwhile
  endif
endfunction

command! -nargs=0 Streamline call Streamline(v:count)
function! Streamline(target)
  let lnum = line('.')
  let lenlnum = len(l:lnum)
  let relativeness = len(l:lnum) - len(a:target)

  let base = l:lnum[: l:relativeness - 1]
  
  let abstarget = l:base.a:target
  if l:abstarget <= l:lnum
    let l:abstarget = (l:base + 1).a:target
  endif
  execute 'normal! '.l:abstarget.'G'
endfunction

command! -nargs=0 StreamlineBack call StreamlineBack(v:count)

function! StreamlineBack(target)
  let lnum = line('.')
  let lenlnum = len(l:lnum)
  let relativeness = len(l:lnum) - len(a:target)
  let base = l:lnum[: l:relativeness - 1]
  
  let abstarget = l:base.a:target
  if l:abstarget >= l:lnum
    let l:abstarget = (l:base - 1).a:target
  endif
  execute 'normal! '.l:abstarget.'G'
endfunction

command! -nargs=1 LineSeek call LineSeek(<f-args>)
command! -nargs=1 LineSeekBack call LineSeekBack(<f-args>)
function! LineSeek(num)
  let lnum = line(".")
  let lennum = len(l:lnum)

  if l:lnum < 10
    if l:lnum[lennum - 1] >= a:num
      let l:base = 1
    else
      let base = 0
    endif
  else 
    let base = l:lnum[: lennum - 2]
    if l:lnum[lennum - 1] >= a:num
      let l:base = l:base + 1
    endif
  endif
  let dest = l:base . a:num
  execute ":".dest
endfunction
function! LineSeekBack(num)
  let lnum = line(".")
  let lennum = len(l:lnum)
  let num = g:seq[ match(g:qes, a:num) ]

  if l:lnum < 10
    if l:lnum[lennum - 1] <= l:num
      execute ":0"
      return
    else
      let base = 0
    endif
  else 
    let base = l:lnum[: lennum - 2]
    if l:lnum[lennum - 1] <= l:num
      let l:base = l:base - 1
    endif
  endif
  let dest = l:base . l:num
  execute ":".dest
endfunction

let reloading = 0
command! -nargs=* AutoReload call AutoReload()
function! AutoReload()
  if l:reloading | ChromeReloadStart<CR>
  else | ChromeReloadStop<CR>
  endif
  :let l:reloading = !l:reloading
endfunction
noremap <Leader>R :w<CR>:ChromeReload<CR>:Vimdow Chrome<CR>
noremap <Leader>r :ChromeReload<CR>

" put all this in your .vimrc or a plugin file
command! -nargs=* Stab call Stab()
function! Stab()
  let l:tabstop = 1 * input('set shiftwidth=')

  if l:tabstop > 0
    " do we want expandtab as well?
    let l:expandtab = confirm('set expandtab?', "&Yes\n&No\n&Cancel")
    if l:expandtab == 3
      " abort?
      return
    endif

    let &l:sts = l:tabstop
    let &l:ts = l:tabstop
    let &l:sw = l:tabstop

    if l:expandtab == 1
      setlocal expandtab
    else
      setlocal noexpandtab
    endif
  endif

  " show the selected options
  try
    echohl ModeMsg
    echon 'set tabstop='
    echohl Question
    echon &l:ts
    echohl ModeMsg
    echon ' shiftwidth='
    echohl Question
    echon &l:sw
    echohl ModeMsg
    echon ' sts='
    echohl Question
    echon &l:sts . ' ' . (&l:et ? '  ' : 'no')
    echohl ModeMsg
    echon 'expandtab'
  finally
    echohl None
  endtry
endfunction

command! -nargs=1 -complete=customlist,DayOpt Day call Day(<f-args>)
function! DayOpt(ArgLead, CmdLine, CursorPos)
  if a:ArgLead == 't' | return ['tue', 'thu'] | endif
  if a:ArgLead == 'tu' | return ['tue'] | endif
  if a:ArgLead == 'th' | return ['thu'] | endif
  if a:ArgLead == 's' | return ['sat', 'sun'] | endif
  if a:ArgLead == 'sa' | return ['sat'] | endif
  if a:ArgLead == 'su' | return ['sun'] | endif
  if a:ArgLead == 'm' || a:ArgLead == 'mo' | return ['mon'] | endif
  if a:ArgLead == 'w' || a:ArgLead == 'we' | return ['wed'] | endif
  if a:ArgLead == 'f' || a:ArgLead == 'fr' | return ['fri'] | endif
  if a:ArgLead == 'u' | return ['tue'] | endif
  if a:ArgLead == 'h' | return ['thu'] | endif
  if a:ArgLead == 'n' | return ['sun'] | endif
  if a:ArgLead == 'a' | return ['sat'] | endif
endfunction

function! Day(date)
  if a:date > 31 && a:date < 1 && !match(
    \ ['mon', 'tue', 'wed', 'thu', 'fri', 'sat', 'sun'], '\c' . a:date)
    return
  endif
  let now = system("date +'%b %-e, %Y 00:00:00 +000 + '")
  let l:now = strpart(l:now, 0, len(l:now) - 1)
  let offset = 86400

  while 1
    let day = system('date --date "' . l:now
      \ . l:offset . ' seconds" ' . "+'%b %-e %a'")
    if match(l:day, '\c\W' . a:date . '\W') != -1
      echo system('date --date "' . l:now
        \ . l:offset . ' seconds" ' . "+'[%Yw%W] %b %-e %a'")
      return
    endif
    let l:offset += 86400
  endwhile
endfunction

command! -nargs=1 Inf call Inform(<f-args>)
function! Inform(data)
  let info = 'No matching info.'
  let otherinfo = []
  if match(['wifi', 'pass'], a:data) != -1 | let l:info = '1251025655'
  elseif match(['phone', 'tel'], a:data) != -1 | let l:info = '3176-6107'

  elseif match(['tar, gz'], a:data) != -1
    let l:info = 'tar xvzf filename.tar.gz'
    call add(l:otherinfo, 'tar it all')
  elseif match(['moon', 'moonc'], a:data) != -1
    let l:info = 'moonc -t "$HOME/.vim/bundle/hudmode-vim/core" .'
    call add(l:otherinfo, 'execute command from ~/goldfeld/hudmode/core')
  elseif match(['restart'], a:data) != -1
    let l:info = 'sudo service network-manager restart'
    call add(l:otherinfo, 'then toggle hardware wireless switch')
  elseif match(['apache', 'stop'], a:data) != -1
    let l:info = 'sudo /etc/init.d/apache2 stop'
    call add(l:otherinfo, 'noop')
  elseif match(['chmod', 'permission', 'executable', 'exe'], a:data) != -1
    let l:info = 'chmod +x filename'
  elseif match(['nodejs', 'node'], a:data) != -1
    let l:info = 'rm -r bin/node bin/node-waf include/node lib/node lib/pkgconfig/nodejs.pc share/man/man1/node.1'
    call add(l:otherinfo, 'noop')


  elseif match(['heroku', 'buildpack'], a:data) != -1
    let appname = input("enter your app's name: ")
    echo "\n"
    let l:info = "heroku create ".l:appname." --stack cedar --buildpack https://github.com/oortcloud/heroku-buildpack-meteorite.git"
    call add(l:otherinfo, "then do 'heroku login'")

  elseif match(['watch', 'inotify'], a:data) != -1
    let l:info = "echo 10000 > /proc/sys/fs/inotify/max_user_watches"
    call add(l:otherinfo, "should 'sudo su' first")
    call add(l:otherinfo, "may also need to pipe to max_user_instances")
    call add(l:otherinfo, "after done, do 'exit'")

  elseif match(['ssh', 'publickey', 'keygen'], a:data) != -1
    let email = input("enter email for publickey: ")
    echo "\n"
    let l:info = 'ssh-keygen -t rsa -C "'.l:email.'"'
    call add(l:otherinfo, "just press enter when prompted for file in which to save")
    call add(l:otherinfo, "use <Leader>.k to xclip the key")
  endif

  let @* = l:info
  let @+ = l:info
  echo l:info
  for other in l:otherinfo
    echo '# '.other
  endfor
  return
endfunction

nnoremap <leader>.p :call ShowingHNParse()<CR>
function! ShowingHNParse()
  let file = '"'. expand('$HOME/Dropbox/showhn') .'"'
  let output = '"'. expand('$HOME/result') .'"'
  let parsed = system("awk 'BEGIN {RS = ".'"[<>]"'."} NR == 2 {print}' ".file."")
  echo parsed
endfunction

nnoremap <leader>.s :call Viminder()<CR>
function! Viminder()
  let [date, time] = split(system("date +'%Y%m%d_%T'"), '_')
  let punchcard = '"' . expand('$HOME') . '/punchcard"'
  echo punchcard
  let awk = "awk '"
    \ . "$1 ~ /".date."/ {print ".'"hey"'.", $0;"
      \ . " print ".'"hay"'.", $0 > ".punchcard."}"
    \ . " $1 !~ /".date."/ {print > ".punchcard."}"
    \ . "' ~/punchcard"
  let sys = system(l:awk)
  echo sys
endfunction

" http://learnvimscriptthehardway.stevelosh.com/chapters/12.html
" http://learnvimscriptthehardway.stevelosh.com/chapters/14.html
" http://forrst.com/posts/Adding_a_Next_Adjective_to_Vim_Version_2-C4P#comment-land
" http://learnvimscriptthehardway.stevelosh.com/chapters/38.html
" https://github.com/amikula/vim_flashcards/blob/master/all_cards.txt

function! LoadSession()
  if filereadable($HOME . "/.vim/Session.vim")
    execute "source " . $HOME . "/.vim/Session.vim"
  endif
endfunction

" save session on exit.
autocmd VimLeave * nested if (!isdirectory($HOME . "/.vim")) |
  \ call mkdir($HOME . "/.vim") |
  \ endif |
  \ execute "mksession! " . $HOME . "/.vim/Session.vim"

" From vimrc_example.vim distributed with Vim 7.
" When editing a file, always jump to the last known cursor position.
" Don't do it when the position is invalid or when inside an event handler
" (happens when dropping a file on gvim).
autocmd BufReadPost *
  \ if line("'\"") > 1 && line("'\"") <= line("$") |
  \   exe "normal! g`\"" |
  \ endif

" Put these in an autocmd group, so that we can delete them easily.
augroup vimrcEx
au!
" For all text files set 'textwidth' to 78 characters.
autocmd FileType text setlocal textwidth=78
" When editing a file, always jump to the last known cursor position.
" Don't do it when the position is invalid or when inside an event handler
" (happens when dropping a file on gvim).
" Also don't do it when the mark is in the first line, that is the default
" position when opening a file.
autocmd BufReadPost *
  \ if line("'\"") > 1 && line("'\"") <= line("$") |
  \   exe "normal! g`\"" |
  \ endif
augroup END

" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
" Only define it when not defined already.
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r ++edit # | 0d_ | diffthis
    \ | wincmd p | diffthis
endif

" search for current selection when in visual mode.
vnoremap <silent> * :call VisualSearch('f')<CR>
vnoremap <silent> # :call VisualSearch('b')<CR>

" from an idea by Michael Naumann
function! VisualSearch(direction) range
  let l:saved_reg = @"
  execute "normal! vgvy"

  let l:pattern = escape(@", '\\/.*$^~[]')
  let l:pattern = substitute(l:pattern, "\n$", "", "")

  if a:direction == 'b' | execute "normal ?" . l:pattern . "^M"
  elseif a:direction == 'gv'
    call CmdLine("vimgrep " . '/'. l:pattern . '/' . ' **/*.')
  elseif a:direction == 'f' | execute "normal /" . l:pattern . "^M"
  endif

  let @/ = l:pattern
  let @" = l:saved_reg
endfunction

" don't mess up splits when resizing vim
autocmd VimResized * wincmd =

"au! CursorHold *.[ch] nested call PreviewWord()
" This will cause a ":ptag" to be executed for the keyword under the cursor,
" when the cursor hasn't moved for the time set with 'updatetime'.  The "nested"
" makes other autocommands be executed, so that syntax highlighting works in the
" preview window.  The "silent!" avoids an error message when the tag could not
" be found.
" A nice addition is to highlight the found tag, avoid the ":ptag" when there
" is no word under the cursor, and a few other things: >
" (from vim documentation)
function! PreviewWord()
  " don't do this in the preview window
  if &previewwindow | return | endif
  let w = expand("<cword>")		" get the word under cursor
  if w =~ '\a'			" if the word contains a letter

    " Delete any existing highlight before showing another tag
    silent! wincmd P			" jump to preview window
    if &previewwindow			" if we really get there...
      match none			" delete existing highlight
      wincmd p			" back to old window
    endif

    " Try displaying a matching tag for the word under the cursor
    try | exe "ptag " . w
    catch | return
    endtry

    silent! wincmd P			" jump to preview window
    if &previewwindow		" if we really get there...
      " don't want a closed fold
      if has("folding") | silent! .foldopen | endif
      call search("$", "b")		" to end of previous line
      let w = substitute(w, '\\', '\\\\', "")
      call search('\<\V' . w . '\>')	" position cursor on match
      " Add a match highlight to the word at this position
      hi previewWord term=bold ctermbg=green guibg=green
      exe 'match previewWord "\%' . line(".") . 'l\%' . col(".") . 'c\k*"'
      wincmd p			" back to old window
    endif
  endif
endfunction
