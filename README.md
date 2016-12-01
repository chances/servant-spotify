# Spotify Web API via servant      [![Build Status](https://travis-ci.org/chances/servant-spotify.svg)](https://travis-ci.org/chances/servant-spotify)

Provides a servant-client based client library for the
[Spotify Web API v1](https://developer.spotify.com/web-api/).

This library defines an incomplete set of [servant](http://hackage.haskell.org/package/servant) types that map to the Spotify Web API (v1).

## Available Endpoints

### Authorization

- GET [authorize](https://developer.spotify.com/web-api/authorization-guide/) - Get an authorization code (via accounts.spotify.com/api)
- POST [token](https://developer.spotify.com/web-api/authorization-guide/) - Get an access token (via accounts.spotify.com/api)

Others coming soon...

## Related Projects

Other Haskell (more complete) client libraries for the Spotify Web API include:

- [spoty](https://hackage.haskell.org/package/spoty) - Provides complete access to all public endpoints, excluding the multi-get versions, powered by lenses and pipes

## License

[MIT License](http://opensource.org/licenses/MIT)

Copyright &copy; 2016 Chance Snow. All rights reserved.
