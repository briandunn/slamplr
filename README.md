# slamplr
A slamin' in browser sampler


## develpment

you need boot.

    https://github.com/boot-clj/boot#install

then do

    boot dev

## Vim REPL with fireplace-vim

This part sucks.

As of the time of this writing you have to use fireplace-vim at [this commit](https://github.com/tpope/vim-fireplace/commit/5866d0017a7f544a27c3f60045958d6d7759b1a8).

boot dev will print out something like

    nREPL server started on port 55468 on host 127.0.0.1 - nrepl://127.0.0.1:55468

in vim do

    :Connect nrepl://127.0.0.1:55468

open `http://localhost:3000` in the browser and check your console. See no errors.

in vim do

    :Piggieback (adzerk.boot-cljs-repl/repl-env)

Check the browser for errors. One that I get a lot is about trying to connect to a websocket with a null port. If this happens, go back to vim and do

    :Piggieback!
    :Piggieback (adzerk.boot-cljs-repl/repl-env)

Maybe that worked. Try to `cpp` a line. If it doesn't print the result, check out your dev server log for exeptions. Broke huh? try redoing random steps from this list. feel frustrated. give up. try one last thing. Bet it started working right when you were to swear off the cljs sauce for keeps this time.
