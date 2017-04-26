## 3.0.1 (26 Apr 2017)

* Use Text instead of String for Beacon dm

## 3.0.0 (26 Apr 2017)

* Add type and dm support for Beacon event

## 2.2.0 (15 Jan 2017)

* Add multicast API support

## 2.1.0.2 (6 Dec 2016)

* Doc and test fixes to use POST for webhooks

## 2.1.0.1 (5 Dec 2016)

* Specify lower bounds in cabal file

## 2.1.0.0 (5 Dec 2016)

* Use `http-conduit` instead of `wreq` as HTTP client

## 2.0.0.0 (4 Dec 2016)

* Make `Line.Messaging.Webhook.Validation` independent from WAI. As it does not
  use `Request` of WAI, its argument type is changed.
* Remove `WebhookResult`, as returning other than empty string is meaningless
  for webhook response
* Add Scotty version of webhook handler
* Add Stack yaml to fix macOS Sierra problem
* Derive `Eq` type class for `APIErrorBody`
* Make optional fields of template messages have type of `Maybe a`

## 1.0.1.0 (28 Nov 2016)

* Update lower bound of `base` to 4.8 (Issue: #2)
* Add Stack yamls for lts-6.26 resolver

## 1.0.0.1 (27 Nov 2016)

* Documentation fix

## 1.0.0.0 (27 Nov 2016)

* Initial release
