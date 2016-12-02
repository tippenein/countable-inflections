# Countable Inflections

[![License MIT](https://img.shields.io/badge/license-MIT-brightgreen.svg)](http://opensource.org/licenses/MIT)
[![Hackage](https://img.shields.io/hackage/v/countable-inflections.svg)](http://hackage.haskell.org/package/countable-inflections)
[![Stackage LTS](http://stackage.org/package/countable-inflections/badge/lts)](http://stackage.org/lts/package/countable-inflections)
[![Build Status](https://circleci.com/gh/tippenein/countable-inflections.svg?style=shield&circle-token=whatever)](https://circleci.com/gh/tippenein/countable-inflections)

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

These can also be given custom inflection matchers

```haskell
λ: :t singularizeWith
[Inflection] -> Text -> Text
```

There are 3 different types of transformations:

### Match

Takes a PCRE regex and a replacement string.

```haskell
λ: :t makeMatchMapping
[(RegexPattern, RegexReplace)] -> [Inflection]

λ: let mapping = makeMatchMapping [("(octop)us", "\\1i")]
λ: pluralizeWith mapping "octopus"
"octopi"
```

### Irregular

From singular to plural with no greater pattern.

```haskell
λ: :t makeIrregularMapping
[(Singular, Plural)] -> [Inflection]

λ: let mapping = makeIrregularMapping [("zombie","zombies")]
λ: pluralizeWith mapping "zombie"
"zombies"
```

### Uncountable

Doesn't have a mapping, word stays the same) so it has the type:

```haskell
[Text] -> [Inflection]
```


## License

MIT - see [the LICENSE file](LICENSE.md).
