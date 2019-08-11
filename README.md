# color-stew
Color combinations generator.

## Build

### Development Build

```sh
$ elm make src/*.elm --output=main.js --debug
```

Or with [elm-live](https://www.npmjs.com/package/elm-live), following command works.

```sh
$ elm-live src/*.elm -- --output=main.js --debug
```

### Production Build

```sh
$ elm make src/*.elm --output=main.js --optimize
```

## License

MIT &copy; 2019, hahnah
