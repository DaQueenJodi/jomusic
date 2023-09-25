pub usingnamespace @cImport({
    @cInclude("taglib/tag_c.h");
    @cInclude("miniaudio.h");
    @cInclude("sqlite3.h");
    // required for readline for some reason
    @cInclude("stdio.h");
    // required for malloc for readline
    @cInclude("stdlib.h");
    @cInclude("readline/history.h");
    @cInclude("readline/readline.h");
});
