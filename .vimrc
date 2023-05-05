" Vim customization for the project, requires 'exrc' option to be set in the
" user vimrc file.
" For now it follows the VScode configuration

let optitrust_dir = expand('<sfile>:p:h')
let optitrust_tools_dir = optitrust_dir . '/tools'

noremap <expr> <F6> ':!'.optitrust_tools_dir.'/view_result.sh "'.expand('%:p:h').'" "'.expand('%:t:r').'" '.line('.').' view_diff recompile_optitrust_no<CR><CR>'
noremap <expr> <S-F6> ':!'.optitrust_tools_dir.'/view_result.sh "'.expand("%:p:h").'" "'.expand('%:t:r').'" '.line('.').' view_diff recompile_optitrust_yes<CR><CR>'
noremap <expr> <C-F6> ':!'.optitrust_tools_dir.'/view_result.sh "'.expand("%:p:h").'" "'.expand('%:t:r').'" '.line('.').' view_trace recompile_optitrust_no<CR><CR>'
noremap <expr> <C-S-F6> ':!'.optitrust_tools_dir.'/view_result.sh "'.expand("%:p:h").'" "'.expand('%:t:r').'" '.line('.').' view_trace recompile_optitrust_yes<CR><CR>'
noremap <expr> <F9> ':!cd "'.expand('%:p:h').'" && make "'.expand('%:t:r').'"<CR>'
noremap <expr> <F10> ':!cd "'.expand('%:p:h').'" && make all<CR>'
