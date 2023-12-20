program DIFF;
var x, y;
begin
    read(x);
    read(y);

    if x <> y then
    begin
        if x >= y then
            x := x - y
        else
            x := y - x;
        write(x);
    end
    else
        write(0)
end.
