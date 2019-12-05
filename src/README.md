# Notes

I'm not here to play golf. Finding the shortest, cleverest answer isn't as much a priority as writing 
code that, if I come back to it later, still tells me what problem it is solving. Hence types over tuples,
descriptive functions, avoiding long chains of combinators, and sticking with attoparsec to decode even 
trivial input.

All solutions are my own work, but after I've solved a problem myself I do go look at other peoples
solutions, and if I see a standard library function that I didn't know about and so wrote the equivalent 
of myself, I'll go back and edit in the library function. (_e.g._ `scanl` in Day3 replaced a hand-written 
longhand equivalent of `scanl`, `any`/`all` in day4 replaced longhand reimplementations of those functions, 
etc.)
