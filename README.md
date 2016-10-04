# Countable Inflections

[![License MIT](https://img.shields.io/badge/license-MIT-brightgreen.svg)](http://opensource.org/licenses/MIT)
[![Hackage](https://img.shields.io/hackage/v/countable.svg)](http://hackage.haskell.org/package/countable)
[![Stackage LTS](http://stackage.org/package/countable/badge/lts)](http://stackage.org/lts/package/countable)
[![Build Status](https://circleci.com/gh/tippenein/countable.svg?style=shield&circle-token=<TOKEN HERE>)](https://circleci.com/gh/tippenein/countable)

This library implements pluralization and singularization in a similar way to the [rails inflectors](http://api.rubyonrails.org/classes/ActiveSupport/Inflector.html)

It uses regexes to define the non-standard transformations and therefore
doesn't provide much safety. If you need to provide the same pluralization and
singularization which rails does out of the box, this will work the same. If
you want _more_ you should be using
[inflections-hs](https://github.com/stackbuilders/inflections-hs) which uses
megaparsec to give you more guarantees

## Usage

```haskell
λ: pluralize "person"
"people"

λ: singularize "branches"
"branch"

```

## License

MIT - see [the LICENSE file](LICENSE.md).
