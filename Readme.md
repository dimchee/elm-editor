# Another code editor written in Elm

- This is my take on rewriting famous Janiczek's
[elm-editor](https://github.com/Janiczek/elm-editor)
from scratch
- Styles are (for now) stolen to preserve same look and feel

## ToDo

- [x] Insert
- [x] Delete
- [ ] Ctrl + backspace for deleting last word
- [ ] tests (+ fuzzy)
- [ ] Change Range to { from: Position, to: Position }
- [ ] Copy paste
- [ ] hover
- [ ] selection
- [ ] vim mode
- [ ] ? context menu
- [ ] dark mode (switch)
- [ ] font size select

## Maybe

- buffer could be array (Token, Range), add Map that maps indices to index in array
- could make 2 way map (from pos to cursor and vice verca)
