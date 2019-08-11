# Color Stew

Color Stew is a design tool for experiments of color combinations.

Try Color Stew at [https://superhahnah.com/application/color-stew](https://superhahnah.com/application/color-stew).

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
