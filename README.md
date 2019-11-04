# American Voter bot 🤖 [Read on Twitter](https://twitter.com/american__voter)

## File dictionary

This repo hosts the data and code for our American Voter bot, a Twitter account that tweets out profiles of real American voters every hour, every day (and it will keep going until ~2036).

- `ccesbot112117.R` cleans and wrangles data from the Cooperative Congressional Election Study and generates the profiles the bot will inject
-  `CCES_ANNOTATED.csv` are the profiles exported by `ccesbot112117.R`
- `bot.py` is the actual Python script that tweets out the random voter biographies
- `index.txt` is a list of keys so the bot knows which profiles it has already tweeted out and doesn't post repeats


## Contact


Contact G. Elliott Morris or Alexander Agadjanian for more. [Elliott's Twitter](http://twitter.com/gelliottmorris) • [Alexander's Twitter](http://twitter.com/a_agadjanian)

## License

MIT License

Copyright (c) 2019 G. Elliott Morris

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.