" Vim customization for the project, requires 'exrc' option to be set in the
" user vimrc file.

let optitrust_dir = expand('<sfile>:p:h')
let optitrust_tools_dir = optitrust_dir . '/tools'

noremap <expr> <F6> ':!'.optitrust_tools_dir.'/view_result.sh "'.expand('%:p:h').'" "'.expand('%:t:r').'" '.line('.').' view_diff<CR><CR>'
noremap <expr> <C-F6> ':!'.optitrust_tools_dir.'/view_result.sh "'.expand("%:p:h").'" "'.expand('%:t:r').'" '.line('.').' view_trace<CR><CR>'
noremap <expr> <F7> ':!'.optitrust_tools_dir.'/_last_view_result.sh<CR>'

"let &makeprg='cd '.optitrust_dir.' && make $*'
"noremap <F5> :lclose<CR>:copen<CR>:make<CR>
"noremap <S-F5> :lclose<CR>:copen<CR>:make tests<CR>

noremap <expr> <F5> ':lclose<CR>:term ++shell cd '.optitrust_dir.' && make<CR>'
noremap <expr> <S-F5> ':lclose<CR>:term ++shell cd '.optitrust_dir.' && make tests<CR>'
