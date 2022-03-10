" Vim customization for the project, requires 'exrc' option to be set in the
" user vimrc file.
" For now it follows the VScode configuration

noremap <expr> <F6> ':!cd .vscode && ./view_result.sh "'.expand("%:p:h").'" "'.expand("%:t:r").'" '.line('.').' view_diff recompile_optitrust_no<CR><CR>'
noremap <expr> <S-F6> ':!cd .vscode && ./view_result.sh "'.expand("%:p:h").'" "'.expand("%:t:r").'" '.line('.').' view_diff recompile_optitrust_yes<CR><CR>'
noremap <expr> <C-F6> ':!cd .vscode && ./view_result.sh "'.expand("%:p:h").'" "'.expand("%:t:r").'" '.line('.').' view_result recompile_optitrust_no<CR><CR>'
noremap <expr> <C-S-F6> ':!cd .vscode && ./view_result.sh "'.expand("%:p:h").'" "'.expand("%:t:r").'" '.line('.').' view_result recompile_optitrust_yes<CR><CR>'
noremap <expr> <F9> ':!cd "'.expand("%:p:h").'" && make "'.expand("%:t:r").'"<CR>'
noremap <expr> <F10> ':!cd "'.expand("%:p:h").'" && make all<CR>'
