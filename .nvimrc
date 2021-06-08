function! s:FindName(path)
  if stridx(a:path, "test/baseline/golden/") == 0
    return split(a:path, "test/baseline/golden/")[0]
  elseif stridx(a:path, "test/baseline/local/") == 0
    return split(a:path, "test/baseline/local/")[0]
  else
    return split(a:path, "test/cases/")[0]
  endif
endfunction

function! s:GetPath(name, prefix, ext)
  return prefix + name + ext
endfunction

function! s:MakePath(prefix, name, ext)
  if a:ext == "tig"
    return "test/cases/".a:name.".tig"
  else
    return a:prefix.a:name.".".a:ext
  endif
endfunction

function! s:TigerLocal(ext)
  let name = s:FindName(expand('%:r'))
  return s:MakePath("test/baseline/local/", name, a:ext)
endfunction

function! s:TigerGolden(ext)
  let name = s:FindName(expand('%:r'))
  return s:MakePath("test/baseline/golden/", name, a:ext)
endfunction

function! s:Open(path)
  execute "e ".a:path
endfunction

function! s:TigerDiff()
  let ext = expand('%:e')
  call s:Open(s:TigerGolden(ext))
  execute "Gdiffsplit ".s:TigerLocal(ext)
endfunction

command! -nargs=* TL call s:Open(s:TigerLocal('<args>'))
command! -nargs=* TG call s:Open(s:TigerGolden('<args>'))
command! -nargs=* TD call s:TigerDiff()

nnoremap <leader>tl :TL 
nnoremap <leader>tg :TG 
nnoremap <leader>td :TD<CR>
