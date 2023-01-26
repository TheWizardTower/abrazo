source ~/.alias

function proot
    while ! test -d ./.git; cd ../; end;
end

# funcsave proot
