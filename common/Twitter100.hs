{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Twitter100
  ( encodedTwitter100
  , byteStringTwitter100
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString,toShort)
import Data.Primitive (ByteArray)
import Data.Text.Encoding (encodeUtf8)
import NeatInterpolation (text)

import qualified Data.Primitive as PM
import qualified Data.ByteString.Short.Internal as BSS

shortByteStringToByteArray :: ShortByteString -> ByteArray 
shortByteStringToByteArray (BSS.SBS x) = PM.ByteArray x

encodedTwitter100 :: ByteArray
encodedTwitter100 =
  shortByteStringToByteArray (toShort byteStringTwitter100)

byteStringTwitter100 :: ByteString
byteStringTwitter100 = encodeUtf8
  [text|
    {
        "completed_in": 1.000000,
        "max_id": 30121530767708160,
        "max_id_str": "30121530767708160",
        "next_page": "?page=2&max_id=30121530767708160&rpp=100&q=haskell",
        "page": 1,
        "query": "haskell",
        "refresh_url": "?since_id=30121530767708160&q=haskell",
        "results": [
            {
                "created_at": "Wed, 26 Jan 2011 04:35:07 +0000",
                "from_user": "nicolaslara",
                "from_user_id": 3646730,
                "from_user_id_str": "3646730",
                "geo": null,
                "id": 30121530767708160,
                "id_str": "30121530767708160",
                "iso_language_code": "es",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a3.twimg.com/profile_images/404973767/avatar_normal.jpg",
                "source": "&lt;a href=&quot;http://twitter.com/&quot; rel=&quot;nofollow&quot;&gt;Twitter for iPhone&lt;/a&gt;",
                "text": "@josej30 Python y Clojure. Obviamente son diferentes, y cada uno tiene sus ventajas y desventajas. De Haskell faltar\u00eda pattern matching",
                "to_user": "josej30",
                "to_user_id": 18616016,
                "to_user_id_str": "18616016"
            },
            {
                "created_at": "Wed, 26 Jan 2011 04:30:38 +0000",
                "from_user": "pboudarga",
                "from_user_id": 207858021,
                "from_user_id_str": "207858021",
                "geo": null,
                "id": 30120402839666689,
                "id_str": "30120402839666689",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a3.twimg.com/sticky/default_profile_images/default_profile_2_normal.png",
                "source": "&lt;a href=&quot;http://foursquare.com&quot; rel=&quot;nofollow&quot;&gt;foursquare&lt;/a&gt;",
                "text": "I'm at Rolla Sushi Grill (27737 Bouquet Canyon Road, #106, Btw Haskell Canyon and Rosedell Drive, Saugus) http://4sq.com/gqqdhs",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 04:25:23 +0000",
                "from_user": "YNK33",
                "from_user_id": 69988683,
                "from_user_id_str": "69988683",
                "geo": null,
                "id": 30119083059978240,
                "id_str": "30119083059978240",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a0.twimg.com/profile_images/1211955817/avatar_7888_normal.gif",
                "source": "&lt;a href=&quot;http://twitterfeed.com&quot; rel=&quot;nofollow&quot;&gt;twitterfeed&lt;/a&gt;",
                "text": "hsndfile 0.5.0: Free and open source Haskell bindings for libsndfile http://bit.ly/gHaBWG Mac Os",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 04:24:28 +0000",
                "from_user": "satzz",
                "from_user_id": 81492,
                "from_user_id_str": "81492",
                "geo": null,
                "id": 30118851488251904,
                "id_str": "30118851488251904",
                "iso_language_code": "ja",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a1.twimg.com/profile_images/423894208/Picture_7_normal.jpg",
                "source": "&lt;a href=&quot;http://www.hootsuite.com&quot; rel=&quot;nofollow&quot;&gt;HootSuite&lt;/a&gt;",
                "text": "Emacs\u306e\u30e2\u30fc\u30c9\u8868\u793a\u304c\u4eca(Ruby Controller Outputz RoR Flymake REl hs)\u3068\u306a\u3063\u3066\u3066\u3088\u304f\u308f\u304b\u3089\u306a\u3044\u3093\u3060\u3051\u3069\u6700\u5f8c\u306eREl\u3068\u304bhs\u3063\u3066\u4f55\u3060\u308d\u3046\u2026haskell\u3068\u304b2\u5e74\u4ee5\u4e0a\u66f8\u3044\u3066\u306a\u3044\u3051\u3069\u2026",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 04:19:19 +0000",
                "from_user": "planet_ocaml",
                "from_user_id": 9518356,
                "from_user_id_str": "9518356",
                "geo": null,
                "id": 30117557788741632,
                "id_str": "30117557788741632",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a2.twimg.com/profile_images/119165723/ocaml-icon_normal.png",
                "source": "&lt;a href=&quot;http://twitterfeed.com&quot; rel=&quot;nofollow&quot;&gt;twitterfeed&lt;/a&gt;",
                "text": "I so miss #haskell type classes in #ocaml - i want to do something like refinement. Also why does ocaml not have... http://bit.ly/geYRwt",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 04:16:32 +0000",
                "from_user": "aprikip",
                "from_user_id": 218059,
                "from_user_id_str": "218059",
                "geo": null,
                "id": 30116854940835840,
                "id_str": "30116854940835840",
                "iso_language_code": "ja",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a1.twimg.com/profile_images/1053837723/twitter-icon9_normal.jpg",
                "source": "&lt;a href=&quot;http://sites.google.com/site/yorufukurou/&quot; rel=&quot;nofollow&quot;&gt;YoruFukurou&lt;/a&gt;",
                "text": "yatex-mode\u3084haskell-mode\u306e\u3053\u3068\u3067\u3059\u306d\u3001\u308f\u304b\u308a\u307e\u3059\u3002",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 04:15:30 +0000",
                "from_user": "dysinger",
                "from_user_id": 216363,
                "from_user_id_str": "216363",
                "geo": null,
                "id": 30116594684264448,
                "id_str": "30116594684264448",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a1.twimg.com/profile_images/72454310/Tim-Avatar_normal.png",
                "source": "&lt;a href=&quot;http://www.nambu.com/&quot; rel=&quot;nofollow&quot;&gt;Nambu&lt;/a&gt;",
                "text": "Haskell in Hawaii tonight for me... #fun",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 04:13:36 +0000",
                "from_user": "DanMil",
                "from_user_id": 1774820,
                "from_user_id_str": "1774820",
                "geo": null,
                "id": 30116117682851840,
                "id_str": "30116117682851840",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a2.twimg.com/profile_images/61169291/dan_desert_thumb_normal.jpg",
                "source": "&lt;a href=&quot;http://twitter.com/&quot;&gt;web&lt;/a&gt;",
                "text": "@ojrac @chewedwire @tomheon Haskell isn't a language, it's a belief system.  A seductive one...",
                "to_user": "ojrac",
                "to_user_id": 1594784,
                "to_user_id_str": "1594784"
            },
            {
                "created_at": "Wed, 26 Jan 2011 04:11:06 +0000",
                "from_user": "djspiewak",
                "from_user_id": 659256,
                "from_user_id_str": "659256",
                "geo": null,
                "id": 30115488931520512,
                "id_str": "30115488931520512",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a0.twimg.com/profile_images/746976711/angular-final_normal.jpg",
                "source": "&lt;a href=&quot;http://itunes.apple.com/us/app/twitter/id409789998?mt=12&quot; rel=&quot;nofollow&quot;&gt;Twitter for Mac&lt;/a&gt;",
                "text": "One of the very nice things about Haskell as opposed to SML is the reduced proliferation of identifiers (e.g. andb, orb, etc). #typeclasses",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 04:06:12 +0000",
                "from_user": "listwarenet",
                "from_user_id": 144546280,
                "from_user_id_str": "144546280",
                "geo": null,
                "id": 30114255890026496,
                "id_str": "30114255890026496",
                "iso_language_code": "no",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a1.twimg.com/a/1295051201/images/default_profile_1_normal.png",
                "source": "&lt;a href=&quot;http://1e10.org/cloud/&quot; rel=&quot;nofollow&quot;&gt;1e10&lt;/a&gt;",
                "text": "http://www.listware.net/201101/haskell-cafe/84752-re-haskell-cafe-gpl-license-of-h-matrix-and-prelude-numeric.html Re: Haskell-c",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 04:01:29 +0000",
                "from_user": "ojrac",
                "from_user_id": 1594784,
                "from_user_id_str": "1594784",
                "geo": null,
                "id": 30113067333324800,
                "id_str": "30113067333324800",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a2.twimg.com/profile_images/378515773/square-profile_normal.jpg",
                "source": "&lt;a href=&quot;http://twitter.com/&quot;&gt;web&lt;/a&gt;",
                "text": "RT @tomheon: @ojrac @chewedwire Don't worry, learning Haskell will not give you any clear idea what monad means.",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 04:00:13 +0000",
                "from_user": "ashleevelazq101",
                "from_user_id": 207589736,
                "from_user_id_str": "207589736",
                "geo": null,
                "id": 30112747555397632,
                "id_str": "30112747555397632",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a3.twimg.com/profile_images/1225527428/headshot_1_normal.jpg",
                "source": "&lt;a href=&quot;http://twitterfeed.com&quot; rel=&quot;nofollow&quot;&gt;twitterfeed&lt;/a&gt;",
                "text": "Federal investigation finds safety violations at The Acadia Hospital: By Meg Haskell, BDN Staff The investigatio... http://bit.ly/dONnpn",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 03:58:00 +0000",
                "from_user": "Hackage",
                "from_user_id": 17671137,
                "from_user_id_str": "17671137",
                "geo": null,
                "id": 30112192346984448,
                "id_str": "30112192346984448",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a2.twimg.com/profile_images/290264834/Haskell-logo-outer-glow_normal.png",
                "source": "&lt;a href=&quot;http://twitterfeed.com&quot; rel=&quot;nofollow&quot;&gt;twitterfeed&lt;/a&gt;",
                "text": "streams 0.4, added by EdwardKmett: Various Haskell 2010 stream comonads http://bit.ly/idBkPe",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 03:58:00 +0000",
                "from_user": "aapnoot",
                "from_user_id": 17489366,
                "from_user_id_str": "17489366",
                "geo": null,
                "id": 30112191881420800,
                "id_str": "30112191881420800",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a3.twimg.com/sticky/default_profile_images/default_profile_2_normal.png",
                "source": "&lt;a href=&quot;http://twitterfeed.com&quot; rel=&quot;nofollow&quot;&gt;twitterfeed&lt;/a&gt;",
                "text": "streams 0.4, added by EdwardKmett: Various Haskell 2010 stream comonads http://bit.ly/idBkPe",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 03:50:12 +0000",
                "from_user": "jeffmclamb",
                "from_user_id": 8530482,
                "from_user_id_str": "8530482",
                "geo": null,
                "id": 30110229207187456,
                "id_str": "30110229207187456",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a2.twimg.com/profile_images/137867266/n608671563_7396_normal.jpg",
                "source": "&lt;a href=&quot;http://friendfeed.com&quot; rel=&quot;nofollow&quot;&gt;FriendFeed&lt;/a&gt;",
                "text": "Angel - daemon to run and monitor processes like daemontools or god, written in Haskell http://ff.im/-wNyLk",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 03:46:01 +0000",
                "from_user": "tomheon",
                "from_user_id": 177539201,
                "from_user_id_str": "177539201",
                "geo": null,
                "id": 30109174645919744,
                "id_str": "30109174645919744",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a0.twimg.com/profile_images/1178368800/img_normal.jpeg",
                "source": "&lt;a href=&quot;http://twitter.com/&quot;&gt;web&lt;/a&gt;",
                "text": "@ojrac @chewedwire Don't worry, learning Haskell will not give you any clear idea what monad means.",
                "to_user": "ojrac",
                "to_user_id": 1594784,
                "to_user_id_str": "1594784"
            },
            {
                "created_at": "Wed, 26 Jan 2011 03:44:34 +0000",
                "from_user": "ojrac",
                "from_user_id": 1594784,
                "from_user_id_str": "1594784",
                "geo": null,
                "id": 30108808684503040,
                "id_str": "30108808684503040",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a2.twimg.com/profile_images/378515773/square-profile_normal.jpg",
                "source": "&lt;a href=&quot;http://twitter.com/&quot;&gt;web&lt;/a&gt;",
                "text": "@chewedwire @tomheon Why are you making me curious about Haskell? I LIKE not knowing what monad means!!",
                "to_user": "chewedwire",
                "to_user_id": 128028225,
                "to_user_id_str": "128028225"
            },
            {
                "created_at": "Wed, 26 Jan 2011 03:41:54 +0000",
                "from_user": "shokalshab",
                "from_user_id": 23750094,
                "from_user_id_str": "23750094",
                "geo": null,
                "id": 30108140443795456,
                "id_str": "30108140443795456",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "place": {
                    "full_name": "Gymnastics Olympica USA, Van Nuys",
                    "id": "4e4a2a2f86cb2946",
                    "type_": "poi"
                },
                "profile_image_url": "http://a0.twimg.com/profile_images/951373780/Moeinthecar_normal.jpg",
                "source": "&lt;a href=&quot;http://foursquare.com&quot; rel=&quot;nofollow&quot;&gt;foursquare&lt;/a&gt;",
                "text": "I'm at Magnitude Cheer @ Gymnastics Olympica USA (7735 Haskell Ave., btw Saticoy &amp; Strathern, Van Nuys) http://4sq.com/gmXfaL",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 03:41:35 +0000",
                "from_user": "Claricei",
                "from_user_id": 8135112,
                "from_user_id_str": "8135112",
                "geo": null,
                "id": 30108059208515584,
                "id_str": "30108059208515584",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a0.twimg.com/profile_images/1165240350/LIMITED_normal.jpg",
                "source": "&lt;a href=&quot;http://www.tweetdeck.com&quot; rel=&quot;nofollow&quot;&gt;TweetDeck&lt;/a&gt;",
                "text": "RT @kristenmchugh22: @KeithOlbermann Ryan looks like Jughead w/o the hat. And sounds as mealy-mouthed as Eddie Haskell.",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 03:40:02 +0000",
                "from_user": "chewedwire",
                "from_user_id": 128028225,
                "from_user_id_str": "128028225",
                "geo": null,
                "id": 30107670367182848,
                "id_str": "30107670367182848",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a1.twimg.com/profile_images/1224994593/Coding_Drunk_normal.jpg",
                "source": "&lt;a href=&quot;http://twitter.com/&quot;&gt;web&lt;/a&gt;",
                "text": "@tomheon Cool, I'll take a look. I feel like I should mention this: http://bit.ly/hVstDM",
                "to_user": "tomheon",
                "to_user_id": 177539201,
                "to_user_id_str": "177539201"
            },
            {
                "created_at": "Wed, 26 Jan 2011 03:38:42 +0000",
                "from_user": "kristenmchugh22",
                "from_user_id": 8679778,
                "from_user_id_str": "8679778",
                "geo": null,
                "id": 30107332381777920,
                "id_str": "30107332381777920",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a3.twimg.com/profile_images/1195318056/me_nyc_12_18_10_icon_normal.jpg",
                "source": "&lt;a href=&quot;http://www.tweetdeck.com&quot; rel=&quot;nofollow&quot;&gt;TweetDeck&lt;/a&gt;",
                "text": "@KeithOlbermann Ryan looks like Jughead w/o the hat. And sounds as mealy-mouthed as Eddie Haskell.",
                "to_user": "KeithOlbermann",
                "to_user_id": 756269,
                "to_user_id_str": "756269"
            },
            {
                "created_at": "Wed, 26 Jan 2011 03:36:15 +0000",
                "from_user": "cityslikr",
                "from_user_id": 103316559,
                "from_user_id_str": "103316559",
                "geo": null,
                "id": 30106719153557504,
                "id_str": "30106719153557504",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a1.twimg.com/profile_images/1179458751/bc3beab4-d59d-4e78-b13b-50747986cfa2_normal.png",
                "source": "&lt;a href=&quot;http://blackberry.com/twitter&quot; rel=&quot;nofollow&quot;&gt;Twitter for BlackBerry\u00ae&lt;/a&gt;",
                "text": "&quot;Social safety net into a hammock.&quot; So says Eddie Haskell with the GOP response. #SOTU",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 03:31:52 +0000",
                "from_user": "wrkforce_safety",
                "from_user_id": 169063143,
                "from_user_id_str": "169063143",
                "geo": null,
                "id": 30105614919143424,
                "id_str": "30105614919143424",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a0.twimg.com/profile_images/1160506212/9697_1_normal.gif",
                "source": "&lt;a href=&quot;http://twitterfeed.com&quot; rel=&quot;nofollow&quot;&gt;twitterfeed&lt;/a&gt;",
                "text": "Federal investigation finds safety violations at The Acadia Hospital: By Meg Haskell, BDN Staff The investigatio... http://bit.ly/gA60C1",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 03:29:40 +0000",
                "from_user": "tomheon",
                "from_user_id": 177539201,
                "from_user_id_str": "177539201",
                "geo": null,
                "id": 30105060960632832,
                "id_str": "30105060960632832",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a0.twimg.com/profile_images/1178368800/img_normal.jpeg",
                "source": "&lt;a href=&quot;http://twitter.com/&quot;&gt;web&lt;/a&gt;",
                "text": "@chewedwire Great book on Haskell: http://oreilly.com/catalog/9780596514983",
                "to_user": "chewedwire",
                "to_user_id": 128028225,
                "to_user_id_str": "128028225"
            },
            {
                "created_at": "Wed, 26 Jan 2011 03:29:19 +0000",
                "from_user": "turnageb",
                "from_user_id": 782692,
                "from_user_id_str": "782692",
                "geo": null,
                "id": 30104974591533057,
                "id_str": "30104974591533057",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a0.twimg.com/profile_images/1031304589/Profile.2007.1_normal.jpg",
                "source": "&lt;a href=&quot;http://www.tweetdeck.com&quot; rel=&quot;nofollow&quot;&gt;TweetDeck&lt;/a&gt;",
                "text": "RT @ovillalon: Paul Ryan solves the mystery of whatever happened to Eddie Haskell. #sotu",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 03:28:04 +0000",
                "from_user": "chewedwire",
                "from_user_id": 128028225,
                "from_user_id_str": "128028225",
                "geo": null,
                "id": 30104657267265536,
                "id_str": "30104657267265536",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a1.twimg.com/profile_images/1224994593/Coding_Drunk_normal.jpg",
                "source": "&lt;a href=&quot;http://twitter.com/&quot;&gt;web&lt;/a&gt;",
                "text": "@tomheon I always loved the pattern matching in SML and it looks like Haskell is MUCH better at it. I'm messing around now at tryhaskell.org",
                "to_user": "tomheon",
                "to_user_id": 177539201,
                "to_user_id_str": "177539201"
            },
            {
                "created_at": "Wed, 26 Jan 2011 03:28:01 +0000",
                "from_user": "ovillalon",
                "from_user_id": 12834082,
                "from_user_id_str": "12834082",
                "geo": null,
                "id": 30104647213518848,
                "id_str": "30104647213518848",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a3.twimg.com/profile_images/1083036140/mugshot_normal.png",
                "source": "&lt;a href=&quot;http://twitter.com/&quot;&gt;web&lt;/a&gt;",
                "text": "Paul Ryan solves the mystery of whatever happened to Eddie Haskell. #sotu",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 03:26:02 +0000",
                "from_user": "goodfox",
                "from_user_id": 13540930,
                "from_user_id_str": "13540930",
                "geo": null,
                "id": 30104146455560192,
                "id_str": "30104146455560192",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a1.twimg.com/profile_images/1205312219/23467_350321383101_821143101_5114147_3010041_n_normal.jpg",
                "source": "&lt;a href=&quot;http://www.ubertwitter.com/bb/download.php&quot; rel=&quot;nofollow&quot;&gt;\u00dcberTwitter&lt;/a&gt;",
                "text": "@billykeene22 Bordeaux is one of my heroes. I was so excited when he accepted the invitation to campus. He's been a great friend to Haskell.",
                "to_user": "billykeene22",
                "to_user_id": 10226179,
                "to_user_id_str": "10226179"
            },
            {
                "created_at": "Wed, 26 Jan 2011 03:25:18 +0000",
                "from_user": "josej30",
                "from_user_id": 18616016,
                "from_user_id_str": "18616016",
                "geo": null,
                "id": 30103962313031681,
                "id_str": "30103962313031681",
                "iso_language_code": "es",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a2.twimg.com/profile_images/1173522726/214397343_normal.jpg",
                "source": "&lt;a href=&quot;http://www.tweetdeck.com&quot; rel=&quot;nofollow&quot;&gt;TweetDeck&lt;/a&gt;",
                "text": "@cris7ian Ahh bueno multiparadigma ya es respetable :) Empezar\u00e9 a explotar la parte funcional de los lenguajes ahora #Haskell",
                "to_user": "Cris7ian",
                "to_user_id": 14870909,
                "to_user_id_str": "14870909"
            },
            {
                "created_at": "Wed, 26 Jan 2011 03:23:43 +0000",
                "from_user": "Cris7ian",
                "from_user_id": 14870909,
                "from_user_id_str": "14870909",
                "geo": null,
                "id": 30103562360983553,
                "id_str": "30103562360983553",
                "iso_language_code": "es",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a2.twimg.com/profile_images/1176930429/oso_yo_normal.png",
                "source": "&lt;a href=&quot;http://www.echofon.com/&quot; rel=&quot;nofollow&quot;&gt;Echofon&lt;/a&gt;",
                "text": "@josej30 hahaha no, es multiparadigma y es bastante lazy. Nothing like haskell, pero s\u00ed, el de Flash",
                "to_user": "josej30",
                "to_user_id": 18616016,
                "to_user_id_str": "18616016"
            },
            {
                "created_at": "Wed, 26 Jan 2011 03:20:24 +0000",
                "from_user": "ernestgrumbles",
                "from_user_id": 2421643,
                "from_user_id_str": "2421643",
                "geo": null,
                "id": 30102730756333568,
                "id_str": "30102730756333568",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a0.twimg.com/profile_images/1190361665/ernestgrumbles-17_normal.jpg",
                "source": "&lt;a href=&quot;http://twitter.com/&quot;&gt;web&lt;/a&gt;",
                "text": "Wow... WolframAlpha did not know who Eddie Haskell is.  Guess I'll never use that &quot;knowledge engine&quot; again.",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 03:14:21 +0000",
                "from_user": "chewedwire",
                "from_user_id": 128028225,
                "from_user_id_str": "128028225",
                "geo": null,
                "id": 30101204428132352,
                "id_str": "30101204428132352",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a1.twimg.com/profile_images/1224994593/Coding_Drunk_normal.jpg",
                "source": "&lt;a href=&quot;http://twitter.com/&quot;&gt;web&lt;/a&gt;",
                "text": "@tomheon How is Haskell better/different from CL or Scheme? I honestly don't know, although I'm becoming more curious.",
                "to_user": "tomheon",
                "to_user_id": 177539201,
                "to_user_id_str": "177539201"
            },
            {
                "created_at": "Wed, 26 Jan 2011 03:06:54 +0000",
                "from_user": "goodfox",
                "from_user_id": 13540930,
                "from_user_id_str": "13540930",
                "geo": null,
                "id": 30099329809129473,
                "id_str": "30099329809129473",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a1.twimg.com/profile_images/1205312219/23467_350321383101_821143101_5114147_3010041_n_normal.jpg",
                "source": "&lt;a href=&quot;http://www.ubertwitter.com/bb/download.php&quot; rel=&quot;nofollow&quot;&gt;\u00dcberTwitter&lt;/a&gt;",
                "text": "A day of vision &amp; speeches. #SOTU now. And a wonderful Haskell Convocation address earlier today by Sinte Gleske President Lionel Bordeaux.",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 03:03:38 +0000",
                "from_user": "LaLiciouz_03",
                "from_user_id": 119185220,
                "from_user_id_str": "119185220",
                "geo": null,
                "id": 30098510623805440,
                "id_str": "30098510623805440",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a0.twimg.com/profile_images/1089027228/dfg_normal.jpg",
                "source": "&lt;a href=&quot;http://twitter.com/&quot;&gt;web&lt;/a&gt;",
                "text": "The Game with the girl room 330 Haskell follow us...",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 02:57:25 +0000",
                "from_user": "josej30",
                "from_user_id": 18616016,
                "from_user_id_str": "18616016",
                "geo": null,
                "id": 30096946144215040,
                "id_str": "30096946144215040",
                "iso_language_code": "es",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a2.twimg.com/profile_images/1173522726/214397343_normal.jpg",
                "source": "&lt;a href=&quot;http://www.tweetdeck.com&quot; rel=&quot;nofollow&quot;&gt;TweetDeck&lt;/a&gt;",
                "text": "Voy a extra\u00f1ar Haskell cuando regrese al mundo imperativo. Hay alg\u00fan lenguaje imperativo que tenga este poder funcional? #ci3661",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 02:55:32 +0000",
                "from_user": "Hackage",
                "from_user_id": 17671137,
                "from_user_id_str": "17671137",
                "geo": null,
                "id": 30096471814574080,
                "id_str": "30096471814574080",
                "iso_language_code": "no",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a2.twimg.com/profile_images/290264834/Haskell-logo-outer-glow_normal.png",
                "source": "&lt;a href=&quot;http://twitterfeed.com&quot; rel=&quot;nofollow&quot;&gt;twitterfeed&lt;/a&gt;",
                "text": "comonad-transformers 0.9.0, added by EdwardKmett: Haskell 98 comonad transformers http://bit.ly/h6xIsf",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 02:48:12 +0000",
                "from_user": "tomheon",
                "from_user_id": 177539201,
                "from_user_id_str": "177539201",
                "geo": null,
                "id": 30094626920603649,
                "id_str": "30094626920603649",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a0.twimg.com/profile_images/1178368800/img_normal.jpeg",
                "source": "&lt;a href=&quot;http://twitter.com/&quot;&gt;web&lt;/a&gt;",
                "text": "Every time I look at Haskell I love it more.",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 02:40:42 +0000",
                "from_user": "r_takaishi",
                "from_user_id": 60792568,
                "from_user_id_str": "60792568",
                "geo": null,
                "id": 30092735935422464,
                "id_str": "30092735935422464",
                "iso_language_code": "ja",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a0.twimg.com/profile_images/1203647517/glenda-flash_normal.jpg",
                "source": "&lt;a href=&quot;http://twmode.sf.net/&quot; rel=&quot;nofollow&quot;&gt;twmode&lt;/a&gt;",
                "text": "Haskell\u3088\u308aD\u8a00\u8a9e\u304c\u4e0a\u3068\u306f\u601d\u308f\u306a\u304b\u3063\u305f\uff0e http://www.tiobe.com/index.php/content/paperinfo/tpci/index.html",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 02:39:31 +0000",
                "from_user": "wtp1787",
                "from_user_id": 160145510,
                "from_user_id_str": "160145510",
                "geo": null,
                "id": 30092439653974018,
                "id_str": "30092439653974018",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a0.twimg.com/profile_images/1218108166/going_galt_normal.jpg",
                "source": "&lt;a href=&quot;http://www.tweetdeck.com&quot; rel=&quot;nofollow&quot;&gt;TweetDeck&lt;/a&gt;",
                "text": "RT @KLSouth: Eddie Haskell Goes to Washington...  &quot;You look really nice tonight, Ms Cleaver&quot;",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 02:39:20 +0000",
                "from_user": "MelissaRNMBA",
                "from_user_id": 96616016,
                "from_user_id_str": "96616016",
                "geo": null,
                "id": 30092392203816960,
                "id_str": "30092392203816960",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a1.twimg.com/profile_images/1196978169/Picture0002_normal.jpg",
                "source": "&lt;a href=&quot;http://twitter.com/&quot;&gt;web&lt;/a&gt;",
                "text": "@KLSouth At least Eddie Haskell was entertaining.",
                "to_user": "KLSouth",
                "to_user_id": 14862975,
                "to_user_id_str": "14862975"
            },
            {
                "created_at": "Wed, 26 Jan 2011 02:38:29 +0000",
                "from_user": "KLSouth",
                "from_user_id": 14862975,
                "from_user_id_str": "14862975",
                "geo": null,
                "id": 30092178327871489,
                "id_str": "30092178327871489",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a0.twimg.com/profile_images/421596393/kls_4_normal.JPG",
                "source": "&lt;a href=&quot;http://www.tweetdeck.com&quot; rel=&quot;nofollow&quot;&gt;TweetDeck&lt;/a&gt;",
                "text": "Eddie Haskell Goes to Washington...  &quot;You look really nice tonight, Ms Cleaver&quot;",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 02:36:17 +0000",
                "from_user": "listwarenet",
                "from_user_id": 144546280,
                "from_user_id_str": "144546280",
                "geo": null,
                "id": 30091626869161984,
                "id_str": "30091626869161984",
                "iso_language_code": "no",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a1.twimg.com/a/1295051201/images/default_profile_1_normal.png",
                "source": "&lt;a href=&quot;http://1e10.org/cloud/&quot; rel=&quot;nofollow&quot;&gt;1e10&lt;/a&gt;",
                "text": "http://www.listware.net/201101/haskell-beginners/84641-haskell-beginners-wildcards-in-expressions.html Haskell-beginners -  Wild",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 02:23:14 +0000",
                "from_user": "dbph",
                "from_user_id": 24538048,
                "from_user_id_str": "24538048",
                "geo": null,
                "id": 30088341030440960,
                "id_str": "30088341030440960",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a2.twimg.com/profile_images/1117267605/rope_normal.jpg",
                "source": "&lt;a href=&quot;http://twitter.com/&quot;&gt;web&lt;/a&gt;",
                "text": "RT @dnene: Skilled Calisthenics. Haskell code that outputs python which spits ruby which emits the haskell source. http://j.mp/YlQUL via @mfeathers",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 01:58:31 +0000",
                "from_user": "YubaVetTech",
                "from_user_id": 158951567,
                "from_user_id_str": "158951567",
                "geo": null,
                "id": 30082124207886336,
                "id_str": "30082124207886336",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a3.twimg.com/profile_images/962721419/Logo_3_normal.jpg",
                "source": "&lt;a href=&quot;http://www.facebook.com/twitter&quot; rel=&quot;nofollow&quot;&gt;Facebook&lt;/a&gt;",
                "text": "Dr. Haskell has just been awarded the prestigious Hayward Award for \u2018Excellence in Education\u2019. This award honors... http://fb.me/QgQCxd74",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 01:56:04 +0000",
                "from_user": "YubaVetTech",
                "from_user_id": 158951567,
                "from_user_id_str": "158951567",
                "geo": null,
                "id": 30081505476743168,
                "id_str": "30081505476743168",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a3.twimg.com/profile_images/962721419/Logo_3_normal.jpg",
                "source": "&lt;a href=&quot;http://www.facebook.com/twitter&quot; rel=&quot;nofollow&quot;&gt;Facebook&lt;/a&gt;",
                "text": "Dr. Haskell has just been awarded the prestigous Haward Award for Excellence in Education. This award honors... http://fb.me/PC9mYmCR",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 01:43:50 +0000",
                "from_user": "Verus",
                "from_user_id": 2331498,
                "from_user_id_str": "2331498",
                "geo": null,
                "id": 30078427155406848,
                "id_str": "30078427155406848",
                "iso_language_code": "ja",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a3.twimg.com/profile_images/494632120/me_normal.jpg",
                "source": "&lt;a href=&quot;http://itunes.apple.com/us/app/twitter/id409789998?mt=12&quot; rel=&quot;nofollow&quot;&gt;Twitter for Mac&lt;/a&gt;",
                "text": "@kami_joe \u3044\u3084\uff0c\u8ab2\u984c\u306f\u89e3\u6c7a\u5bfe\u8c61(\u30d1\u30ba\u30eb\u3068\u304b)\u3092\u4e0e\u3048\u3089\u308c\u3066\uff0c\u554f\u984c\u5b9a\u7fa9\u3068\u30d7\u30ed\u30b0\u30e9\u30df\u30f3\u30b0(\u6307\u5b9a\u8a00\u8a9e\u306fC++\u3082\u3057\u304f\u306fJava)\u3068\u3044\u3046\u3044\u308f\u3070\u666e\u901a\u306a\u8ab2\u984c\u3067\u306f\u3042\u308b\u3093\u3060\u3051\u3069\uff0e\u95a2\u6570\u578b\u8a00\u8a9e\u306fHaskell\u306e\u6388\u696d\u304c\u307e\u305f\u5225\u306b\u3042\u308b\u306e\uff0e",
                "to_user": "kami_joe",
                "to_user_id": 79273052,
                "to_user_id_str": "79273052"
            },
            {
                "created_at": "Wed, 26 Jan 2011 01:40:37 +0000",
                "from_user": "cz_newdrafts",
                "from_user_id": 79151233,
                "from_user_id_str": "79151233",
                "geo": null,
                "id": 30077617361125376,
                "id_str": "30077617361125376",
                "iso_language_code": "no",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a2.twimg.com/a/1295051201/images/default_profile_2_normal.png",
                "source": "&lt;a href=&quot;http://tommorris.org/&quot; rel=&quot;nofollow&quot;&gt;tommorris' hacksample&lt;/a&gt;",
                "text": "Haskell programming language http://bit.ly/gpPAwB",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 01:02:57 +0000",
                "from_user": "Verus",
                "from_user_id": 2331498,
                "from_user_id_str": "2331498",
                "geo": null,
                "id": 30068139416879104,
                "id_str": "30068139416879104",
                "iso_language_code": "ja",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a3.twimg.com/profile_images/494632120/me_normal.jpg",
                "source": "&lt;a href=&quot;http://itunes.apple.com/us/app/twitter/id409789998?mt=12&quot; rel=&quot;nofollow&quot;&gt;Twitter for Mac&lt;/a&gt;",
                "text": "@shukukei Java\u3068C\u306f\u3042\u308b\u7a0b\u5ea6\u66f8\u3051\u3066\u3042\u305f\u308a\u307e\u3048\u306a\u3068\u3053\u308d\u304c\u3042\u308b\u304b\u3089\u306a\u30fc\uff0ePython\u306f\u500b\u4eba\u7684\u306b\u611f\u899a\u304c\u5408\u308f\u306a\u3044\uff0e\u611f\u899a\u306a\u306e\u3067\uff0c\u3082\u3046\u3069\u3046\u3057\u3088\u3046\u3082\u306a\u3044\uff57 \u3044\u307e\u306fScala\u3068Haskell\u3092\u3082\u3063\u3068\u6975\u3081\u305f\u3044\u3068\u3053\u308d\uff0e",
                "to_user": "shukukei",
                "to_user_id": 9252720,
                "to_user_id_str": "9252720"
            },
            {
                "created_at": "Wed, 26 Jan 2011 01:02:35 +0000",
                "from_user": "joeyhess",
                "from_user_id": 2781460,
                "from_user_id_str": "2781460",
                "geo": null,
                "id": 30068046538219520,
                "id_str": "30068046538219520",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a0.twimg.com/profile_images/82526625/.joeyicon_normal.jpg",
                "source": "&lt;a href=&quot;http://identi.ca&quot; rel=&quot;nofollow&quot;&gt;identica&lt;/a&gt;",
                "text": "just figured out that I can use parameterized types to remove a dependency loop in git-annex's type definitions. whee #haskell",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 00:54:22 +0000",
                "from_user": "listwarenet",
                "from_user_id": 144546280,
                "from_user_id_str": "144546280",
                "geo": null,
                "id": 30065977920061440,
                "id_str": "30065977920061440",
                "iso_language_code": "no",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a1.twimg.com/a/1295051201/images/default_profile_1_normal.png",
                "source": "&lt;a href=&quot;http://1e10.org/cloud/&quot; rel=&quot;nofollow&quot;&gt;1e10&lt;/a&gt;",
                "text": "http://www.listware.net/201101/haskell-beginners/84466-haskell-beginners-bytestring-question.html Haskell-beginners -  Bytestrin",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 00:52:07 +0000",
                "from_user": "_aaron_",
                "from_user_id": 1291845,
                "from_user_id_str": "1291845",
                "geo": null,
                "id": 30065412242669568,
                "id_str": "30065412242669568",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a0.twimg.com/profile_images/1225743404/ThinOxygen-small-opaque-solidarity_normal.png",
                "source": "&lt;a href=&quot;http://twitter.com/&quot;&gt;web&lt;/a&gt;",
                "text": "wanted: librly licensed high level native compiled lang with min runtime (otherwise cobra/mono would be perfect) for win. lua? haskell? ooc?",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 00:45:45 +0000",
                "from_user": "shelarcy",
                "from_user_id": 5912444,
                "from_user_id_str": "5912444",
                "geo": null,
                "id": 30063810773516288,
                "id_str": "30063810773516288",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a2.twimg.com/profile_images/546261026/ssf0xg11_normal.jpg",
                "source": "&lt;a href=&quot;http://twitterfeed.com&quot; rel=&quot;nofollow&quot;&gt;twitterfeed&lt;/a&gt;",
                "text": "RT @Hackage: download 0.3.1.1, added by MagnusTherning: High-level file download based on URLs http://bit.ly/eHfiJB",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 00:41:00 +0000",
                "from_user": "kudzu_naoki",
                "from_user_id": 135993,
                "from_user_id_str": "135993",
                "geo": null,
                "id": 30062613287141376,
                "id_str": "30062613287141376",
                "iso_language_code": "ja",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a2.twimg.com/profile_images/1190667086/myface2010small_normal.jpg",
                "source": "&lt;a href=&quot;http://www.hootsuite.com&quot; rel=&quot;nofollow&quot;&gt;HootSuite&lt;/a&gt;",
                "text": "RT @tanakh: \u9b54\u6cd5Haskell\u5c11\u5973\u5019\u88dc\u3092\u63a2\u3059\u306e\u3082\u5927\u5909\u306a\u3093\u3060",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 00:40:25 +0000",
                "from_user": "omasanori",
                "from_user_id": 52620546,
                "from_user_id_str": "52620546",
                "geo": null,
                "id": 30062465656033280,
                "id_str": "30062465656033280",
                "iso_language_code": "ja",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a1.twimg.com/profile_images/434693356/twitter-icon_normal.png",
                "source": "&lt;a href=&quot;http://www.movatwi.jp&quot; rel=&quot;nofollow&quot;&gt;www.movatwi.jp&lt;/a&gt;",
                "text": "\u9b54\u6cd5Haskell\u5c11\u5973\u306e\u4e00\u65e5\u306fGHC HEAD\u306e\u30d3\u30eb\u30c9\u304b\u3089\u59cb\u307e\u308b",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 00:37:12 +0000",
                "from_user": "omasanori",
                "from_user_id": 52620546,
                "from_user_id_str": "52620546",
                "geo": null,
                "id": 30061656331526144,
                "id_str": "30061656331526144",
                "iso_language_code": "ja",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a1.twimg.com/profile_images/434693356/twitter-icon_normal.png",
                "source": "&lt;a href=&quot;http://www.hootsuite.com&quot; rel=&quot;nofollow&quot;&gt;HootSuite&lt;/a&gt;",
                "text": "RT @tanakh: \u50d5\u3068\u5951\u7d04\u3057\u3066\u9b54\u6cd5Haskell\u5c11\u5973\u306b\u306a\u308d\u3046\u3088\uff01",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 00:35:42 +0000",
                "from_user": "kudzu_naoki",
                "from_user_id": 135993,
                "from_user_id_str": "135993",
                "geo": null,
                "id": 30061282665168896,
                "id_str": "30061282665168896",
                "iso_language_code": "ja",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a2.twimg.com/profile_images/1190667086/myface2010small_normal.jpg",
                "source": "&lt;a href=&quot;http://www.hootsuite.com&quot; rel=&quot;nofollow&quot;&gt;HootSuite&lt;/a&gt;",
                "text": "RT @tanakh: \u50d5\u3068\u5951\u7d04\u3057\u3066\u9b54\u6cd5Haskell\u5c11\u5973\u306b\u306a\u308d\u3046\u3088\uff01",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 00:30:02 +0000",
                "from_user": "qnighy",
                "from_user_id": 1631333,
                "from_user_id_str": "1631333",
                "geo": null,
                "id": 30059855884582913,
                "id_str": "30059855884582913",
                "iso_language_code": "ja",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a1.twimg.com/profile_images/81268862/profile300_normal.jpg",
                "source": "&lt;a href=&quot;http://www.hootsuite.com&quot; rel=&quot;nofollow&quot;&gt;HootSuite&lt;/a&gt;",
                "text": "RT @tanakh: \u50d5\u3068\u5951\u7d04\u3057\u3066\u9b54\u6cd5Haskell\u5c11\u5973\u306b\u306a\u308d\u3046\u3088\uff01",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 00:26:20 +0000",
                "from_user": "phdwine",
                "from_user_id": 9093754,
                "from_user_id_str": "9093754",
                "geo": null,
                "id": 30058925516656640,
                "id_str": "30058925516656640",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a1.twimg.com/profile_images/99435906/PHD_3d_col_blk_normal.jpg",
                "source": "&lt;a href=&quot;http://www.tweetdeck.com&quot; rel=&quot;nofollow&quot;&gt;TweetDeck&lt;/a&gt;",
                "text": "Last weekend of Tri Nations Tasting with PHD wines at Haskell Vineyards. Hope you didn't miss the Pinot Noir tasting last week !",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 00:21:14 +0000",
                "from_user": "djay75",
                "from_user_id": 2119923,
                "from_user_id_str": "2119923",
                "geo": null,
                "id": 30057639543050240,
                "id_str": "30057639543050240",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a2.twimg.com/profile_images/1096701078/djayprofile_normal.jpg",
                "source": "&lt;a href=&quot;http://twitter.com/&quot;&gt;web&lt;/a&gt;",
                "text": "RT @dnene: Skilled Calisthenics. Haskell code that outputs python which spits ruby which emits the haskell source. http://j.mp/YlQUL via @mfeathers",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 00:19:30 +0000",
                "from_user": "PrinceOfBrkeley",
                "from_user_id": 134323731,
                "from_user_id_str": "134323731",
                "geo": null,
                "id": 30057205554216960,
                "id_str": "30057205554216960",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a2.twimg.com/profile_images/1225818824/IMG00518-20110125-1547_normal.jpg",
                "source": "&lt;a href=&quot;http://blackberry.com/twitter&quot; rel=&quot;nofollow&quot;&gt;Twitter for BlackBerry\u00ae&lt;/a&gt;",
                "text": "@PayThaPrince go to haskell.",
                "to_user": "PayThaPrince",
                "to_user_id": 167093027,
                "to_user_id_str": "167093027"
            },
            {
                "created_at": "Wed, 26 Jan 2011 00:03:28 +0000",
                "from_user": "bennadel",
                "from_user_id": 705857,
                "from_user_id_str": "705857",
                "geo": null,
                "id": 30053166972141569,
                "id_str": "30053166972141569",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a1.twimg.com/profile_images/429483912/ben_twitter_normal.jpg",
                "source": "&lt;a href=&quot;http://www.tweetdeck.com&quot; rel=&quot;nofollow&quot;&gt;TweetDeck&lt;/a&gt;",
                "text": "Using #Homebrew to install Haskell - the last of the Seven Languages (in Seven Weeks) http://bit.ly/eA9Cv1 This has been some journey.",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 00:02:54 +0000",
                "from_user": "simonszu",
                "from_user_id": 251466,
                "from_user_id_str": "251466",
                "geo": null,
                "id": 30053025502466048,
                "id_str": "30053025502466048",
                "iso_language_code": "de",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a2.twimg.com/profile_images/1194100934/pass_normal.jpg",
                "source": "&lt;a href=&quot;http://www.echofon.com/&quot; rel=&quot;nofollow&quot;&gt;Echofon&lt;/a&gt;",
                "text": "Ouh yeah! Ich werde doch produktiv sein, diese Semesterferien. Ich werde funktional Programmieren lernen. #haskell, Baby.",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Tue, 25 Jan 2011 23:59:13 +0000",
                "from_user": "diligiant",
                "from_user_id": 357786,
                "from_user_id_str": "357786",
                "geo": null,
                "id": 30052100973006848,
                "id_str": "30052100973006848",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a0.twimg.com/profile_images/612044841/FM_2010_normal.jpg",
                "source": "&lt;a href=&quot;http://itunes.apple.com/us/app/twitter/id409789998?mt=12&quot; rel=&quot;nofollow&quot;&gt;Twitter for Mac&lt;/a&gt;",
                "text": "RT @paulrbrown: &quot;...we only discovered a single bug after compilation.&quot; (http://t.co/K0wlEYz) #haskell",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Tue, 25 Jan 2011 23:54:46 +0000",
                "from_user": "wlad_kent",
                "from_user_id": 101597523,
                "from_user_id_str": "101597523",
                "geo": null,
                "id": 30050977964892160,
                "id_str": "30050977964892160",
                "iso_language_code": "pt",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a1.twimg.com/profile_images/1206183256/2010-12-12-204902_normal.jpg",
                "source": "&lt;a href=&quot;http://twitter.com/&quot;&gt;web&lt;/a&gt;",
                "text": "@Nilson_Neto Isso eh bem basico. quando vc estiver estudando recurs\u00e3o em Haskell, ai vc vai ver oq eh papo de nerd - kkk - 1\u00ba periodo isso.",
                "to_user": "Nilson_Neto",
                "to_user_id": 86297184,
                "to_user_id_str": "86297184"
            },
            {
                "created_at": "Tue, 25 Jan 2011 23:51:22 +0000",
                "from_user": "listwarenet",
                "from_user_id": 144546280,
                "from_user_id_str": "144546280",
                "geo": null,
                "id": 30050123383832577,
                "id_str": "30050123383832577",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a1.twimg.com/a/1295051201/images/default_profile_1_normal.png",
                "source": "&lt;a href=&quot;http://1e10.org/cloud/&quot; rel=&quot;nofollow&quot;&gt;1e10&lt;/a&gt;",
                "text": "http://www.listware.net/201101/haskell-cafe/84336-haskell-cafe-monomorphic-let-bindings-and-darcs.html Haskell-cafe -  Monomorph",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Tue, 25 Jan 2011 23:48:40 +0000",
                "from_user": "lsbardel",
                "from_user_id": 5776901,
                "from_user_id_str": "5776901",
                "geo": null,
                "id": 30049446469312512,
                "id_str": "30049446469312512",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a0.twimg.com/profile_images/472682157/lucabw_normal.JPG",
                "source": "&lt;a href=&quot;http://www.tweetdeck.com&quot; rel=&quot;nofollow&quot;&gt;TweetDeck&lt;/a&gt;",
                "text": "Very interesting technology stack these guys use http://bit.ly/ghAjS7 #python #redis #haskell",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Tue, 25 Jan 2011 23:45:48 +0000",
                "from_user": "pmonks",
                "from_user_id": 3005901,
                "from_user_id_str": "3005901",
                "geo": null,
                "id": 30048721110573056,
                "id_str": "30048721110573056",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a0.twimg.com/profile_images/278943006/Lone_Pine_Peak_-_Peter_200x200_normal.jpg",
                "source": "&lt;a href=&quot;http://www.tweetdeck.com&quot; rel=&quot;nofollow&quot;&gt;TweetDeck&lt;/a&gt;",
                "text": "@techielicous Haskell: http://bit.ly/gy3oFi  ;-)",
                "to_user": "techielicous",
                "to_user_id": 203834278,
                "to_user_id_str": "203834278"
            },
            {
                "created_at": "Tue, 25 Jan 2011 23:44:47 +0000",
                "from_user": "TheRonaldMCD",
                "from_user_id": 162697074,
                "from_user_id_str": "162697074",
                "geo": null,
                "id": 30048468781244416,
                "id_str": "30048468781244416",
                "iso_language_code": "no",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a2.twimg.com/profile_images/826205838/ryan3_normal.jpg",
                "source": "&lt;a href=&quot;http://foursquare.com&quot; rel=&quot;nofollow&quot;&gt;foursquare&lt;/a&gt;",
                "text": "I'm at Aldi Food Market (4122 Gaston Ave, Haskell, Dallas) http://4sq.com/ekqRY7",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Tue, 25 Jan 2011 23:44:04 +0000",
                "from_user": "techielicous",
                "from_user_id": 203834278,
                "from_user_id_str": "203834278",
                "geo": null,
                "id": 30048286589067264,
                "id_str": "30048286589067264",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a0.twimg.com/profile_images/1223375080/me_normal.jpg",
                "source": "&lt;a href=&quot;http://www.tweetdeck.com&quot; rel=&quot;nofollow&quot;&gt;TweetDeck&lt;/a&gt;",
                "text": "@pmonks I was thinking Haskell or Scheme",
                "to_user": "pmonks",
                "to_user_id": 3005901,
                "to_user_id_str": "3005901"
            },
            {
                "created_at": "Tue, 25 Jan 2011 23:38:14 +0000",
                "from_user": "nushio",
                "from_user_id": 537690,
                "from_user_id_str": "537690",
                "geo": null,
                "id": 30046818658164737,
                "id_str": "30046818658164737",
                "iso_language_code": "ja",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a3.twimg.com/profile_images/45665122/nushiostamp_normal.jpg",
                "source": "&lt;a href=&quot;http://twitter.com/&quot;&gt;web&lt;/a&gt;",
                "text": "\u305d\u3046\u3044\u3084\u5951\u7d04\u306b\u3088\u308b\u30d7\u30ed\u30b0\u30e9\u30df\u30f3\u30b0\u3068\u304b\u3042\u3063\u305f\u306a\u3002Haskell\u306a\u3093\u304b\u3088\u308a\u3042\u3063\u3061\u306e\u65b9\u304c\u9b54\u6cd5\u5c11\u5973\u547c\u3070\u308f\u308a\u306b\u3075\u3055\u308f\u3057\u3044\u3002",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Tue, 25 Jan 2011 23:38:03 +0000",
                "from_user": "aodag",
                "from_user_id": 181192,
                "from_user_id_str": "181192",
                "geo": null,
                "id": 30046772223016960,
                "id_str": "30046772223016960",
                "iso_language_code": "ja",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a2.twimg.com/profile_images/1089890701/q530888600_9828_normal.jpg",
                "source": "&lt;a href=&quot;http://www.movatwi.jp&quot; rel=&quot;nofollow&quot;&gt;www.movatwi.jp&lt;/a&gt;",
                "text": "RT @omasanori: \u300c\u3082\u3046\u4f55\u5e74\u3082Haskell\u306e\u3053\u3068\u3057\u304b\u8003\u3048\u3066\u306a\u304b\u3063\u305f\u304b\u3089\u306a\u3001\u30eb\u30fc\u30d7\u306e\u4f7f\u3044\u65b9\u5fd8\u308c\u3066\u305f\u300d",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Tue, 25 Jan 2011 23:37:53 +0000",
                "from_user": "kanaya",
                "from_user_id": 5886055,
                "from_user_id_str": "5886055",
                "geo": null,
                "id": 30046729243983873,
                "id_str": "30046729243983873",
                "iso_language_code": "ja",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a3.twimg.com/profile_images/85531669/ichi_normal.jpg",
                "source": "&lt;a href=&quot;http://itunes.apple.com/app/twitter/id333903271?mt=8&quot; rel=&quot;nofollow&quot;&gt;Twitter for iPad&lt;/a&gt;",
                "text": "\u9e97\u3057\u3044\u3002\u201c@omasanori: \u300c\u3082\u3046\u4f55\u5e74\u3082Haskell\u306e\u3053\u3068\u3057\u304b\u8003\u3048\u3066\u306a\u304b\u3063\u305f\u304b\u3089\u306a\u3001\u30eb\u30fc\u30d7\u306e\u4f7f\u3044\u65b9\u5fd8\u308c\u3066\u305f\u300d\u201d",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Tue, 25 Jan 2011 23:36:26 +0000",
                "from_user": "omasanori",
                "from_user_id": 52620546,
                "from_user_id_str": "52620546",
                "geo": null,
                "id": 30046367187476481,
                "id_str": "30046367187476481",
                "iso_language_code": "ja",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a1.twimg.com/profile_images/434693356/twitter-icon_normal.png",
                "source": "&lt;a href=&quot;http://www.movatwi.jp&quot; rel=&quot;nofollow&quot;&gt;www.movatwi.jp&lt;/a&gt;",
                "text": "\u300c\u3082\u3046\u4f55\u5e74\u3082Haskell\u306e\u3053\u3068\u3057\u304b\u8003\u3048\u3066\u306a\u304b\u3063\u305f\u304b\u3089\u306a\u3001\u30eb\u30fc\u30d7\u306e\u4f7f\u3044\u65b9\u5fd8\u308c\u3066\u305f\u300d",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Tue, 25 Jan 2011 23:16:13 +0000",
                "from_user": "finalfusion",
                "from_user_id": 1659992,
                "from_user_id_str": "1659992",
                "geo": null,
                "id": 30041278544609280,
                "id_str": "30041278544609280",
                "iso_language_code": "ja",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a1.twimg.com/profile_images/504488750/1_normal.jpg",
                "source": "&lt;a href=&quot;http://twitter.com/&quot; rel=&quot;nofollow&quot;&gt;Twitter for iPhone&lt;/a&gt;",
                "text": "RT @necocen: Haskell\u306e\u30df\u30b5\u30ef\u3001\u3068\u3044\u3046\u3082\u306e\u3092\u8003\u3048\u3066\u307f\u3066\u3071\u3063\u3068\u3057\u306a\u3044",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Tue, 25 Jan 2011 23:05:09 +0000",
                "from_user": "mikehadlow",
                "from_user_id": 272513,
                "from_user_id_str": "272513",
                "geo": null,
                "id": 30038492373319680,
                "id_str": "30038492373319680",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a2.twimg.com/profile_images/320244078/Mike_Painting2_normal.png",
                "source": "&lt;a href=&quot;http://www.tweetdeck.com&quot; rel=&quot;nofollow&quot;&gt;TweetDeck&lt;/a&gt;",
                "text": "Just written the same simple program in C#, F# and Haskell: 18, 12 and 9 LoC respectively. And I'm much better at C# than F# or Haskell.",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Tue, 25 Jan 2011 23:01:28 +0000",
                "from_user": "newsportlandme",
                "from_user_id": 89393264,
                "from_user_id_str": "89393264",
                "geo": null,
                "id": 30037564589084673,
                "id_str": "30037564589084673",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a0.twimg.com/profile_images/1169486472/020_portland_me_normal.jpg",
                "source": "&lt;a href=&quot;http://twitterfeed.com&quot; rel=&quot;nofollow&quot;&gt;twitterfeed&lt;/a&gt;",
                "text": "Governor's Choice to Run Maine DOC Under Scrutiny - MPBN: State Rep. Anne Haskell, a Portland Democrat, says Mai... http://bit.ly/fO30gC",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Tue, 25 Jan 2011 22:59:55 +0000",
                "from_user": "michaelneale",
                "from_user_id": 38138,
                "from_user_id_str": "38138",
                "geo": null,
                "id": 30037174615281664,
                "id_str": "30037174615281664",
                "iso_language_code": "no",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a3.twimg.com/profile_images/1212565885/Screen_shot_2011-01-11_at_5.28.47_PM_normal.png",
                "source": "&lt;a href=&quot;http://itunes.apple.com/us/app/twitter/id409789998?mt=12&quot; rel=&quot;nofollow&quot;&gt;Twitter for Mac&lt;/a&gt;",
                "text": "The haskell EvilMangler http://t.co/P2Tls7J",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Tue, 25 Jan 2011 22:45:06 +0000",
                "from_user": "karno",
                "from_user_id": 45692,
                "from_user_id_str": "45692",
                "geo": null,
                "id": 30033448286552064,
                "id_str": "30033448286552064",
                "iso_language_code": "ja",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a3.twimg.com/profile_images/1177741507/dogkarno_r_normal.jpg",
                "source": "&lt;a href=&quot;http://yubitter.com/&quot; rel=&quot;nofollow&quot;&gt;yubitter&lt;/a&gt;",
                "text": "Haskell\u3063\u3066\u95a2\u6570\u306b\u30ab\u30ec\u30fc\u7c89\u3076\u3061\u8fbc\u3080\u3068\u304b\u306a\u3093\u3068\u304b",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Tue, 25 Jan 2011 22:44:23 +0000",
                "from_user": "celeron1ghz",
                "from_user_id": 27605,
                "from_user_id_str": "27605",
                "geo": null,
                "id": 30033268761956352,
                "id_str": "30033268761956352",
                "iso_language_code": "ja",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a2.twimg.com/profile_images/15826632/meron_normal.jpg",
                "source": "&lt;a href=&quot;http://twitter.com/&quot; rel=&quot;nofollow&quot;&gt;Twitter for iPhone&lt;/a&gt;",
                "text": "RT @necocen: Haskell\u306e\u30df\u30b5\u30ef\u3001\u3068\u3044\u3046\u3082\u306e\u3092\u8003\u3048\u3066\u307f\u3066\u3071\u3063\u3068\u3057\u306a\u3044",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Tue, 25 Jan 2011 22:43:30 +0000",
                "from_user": "necocen",
                "from_user_id": 12901,
                "from_user_id_str": "12901",
                "geo": null,
                "id": 30033044035346432,
                "id_str": "30033044035346432",
                "iso_language_code": "ja",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a1.twimg.com/profile_images/56091349/haruhi_nagato-Y_normal.jpg",
                "source": "&lt;a href=&quot;http://twitter.com/&quot; rel=&quot;nofollow&quot;&gt;Twitter for iPhone&lt;/a&gt;",
                "text": "\u307e\u3042Haskell\u77e5\u3089\u3093\u3057\u306d",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Tue, 25 Jan 2011 22:42:54 +0000",
                "from_user": "necocen",
                "from_user_id": 12901,
                "from_user_id_str": "12901",
                "geo": null,
                "id": 30032894856531968,
                "id_str": "30032894856531968",
                "iso_language_code": "ja",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a1.twimg.com/profile_images/56091349/haruhi_nagato-Y_normal.jpg",
                "source": "&lt;a href=&quot;http://twitter.com/&quot; rel=&quot;nofollow&quot;&gt;Twitter for iPhone&lt;/a&gt;",
                "text": "Haskell\u306e\u30df\u30b5\u30ef\u3001\u3068\u3044\u3046\u3082\u306e\u3092\u8003\u3048\u3066\u307f\u3066\u3071\u3063\u3068\u3057\u306a\u3044",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Tue, 25 Jan 2011 22:35:09 +0000",
                "from_user": "jjedMoriAnktah",
                "from_user_id": 10821270,
                "from_user_id_str": "10821270",
                "geo": null,
                "id": 30030942106025984,
                "id_str": "30030942106025984",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a0.twimg.com/profile_images/1195474259/Salto_Marlon_normal.jpg",
                "source": "&lt;a href=&quot;http://www.tweetdeck.com&quot; rel=&quot;nofollow&quot;&gt;TweetDeck&lt;/a&gt;",
                "text": "&quot;A Haskell program that outputs a Python program that outputs a Ruby program that outputs the original Haskell program&quot; http://is.gd/E6Julu",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Tue, 25 Jan 2011 22:34:30 +0000",
                "from_user": "comatose_kid",
                "from_user_id": 7554762,
                "from_user_id_str": "7554762",
                "geo": null,
                "id": 30030780205899776,
                "id_str": "30030780205899776",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a0.twimg.com/profile_images/100515212/ajay-photo_normal.jpg",
                "source": "&lt;a href=&quot;http://twitter.com/&quot;&gt;web&lt;/a&gt;",
                "text": "RT @bumptech: Bump Dev Blog - Why we use Haskell at Bump http://devblog.bu.mp/haskell-at-bump",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Tue, 25 Jan 2011 22:29:19 +0000",
                "from_user": "landmvintage",
                "from_user_id": 148474705,
                "from_user_id_str": "148474705",
                "geo": null,
                "id": 30029476003840000,
                "id_str": "30029476003840000",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a0.twimg.com/profile_images/1119234716/fur_hat_-_gilbeys_vodka_-_life_-_11-30-1962_normal.JPG",
                "source": "&lt;a href=&quot;http://twitter.com/&quot;&gt;web&lt;/a&gt;",
                "text": "RT @faerymoongodess: Magnificent Miriam Haskell Baroque Pearl Necklace by @MercyMadge http://etsy.me/hqRl51",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Tue, 25 Jan 2011 22:24:52 +0000",
                "from_user": "faerymoongodess",
                "from_user_id": 37715508,
                "from_user_id_str": "37715508",
                "geo": null,
                "id": 30028356326002688,
                "id_str": "30028356326002688",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a1.twimg.com/profile_images/1207891411/ballerina_normal.jpg",
                "source": "&lt;a href=&quot;http://twitter.com/&quot;&gt;web&lt;/a&gt;",
                "text": "Magnificent Miriam Haskell Baroque Pearl Necklace by @MercyMadge http://etsy.me/hqRl51",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Tue, 25 Jan 2011 22:17:16 +0000",
                "from_user": "Hackage",
                "from_user_id": 17671137,
                "from_user_id_str": "17671137",
                "geo": null,
                "id": 30026442456694784,
                "id_str": "30026442456694784",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a2.twimg.com/profile_images/290264834/Haskell-logo-outer-glow_normal.png",
                "source": "&lt;a href=&quot;http://twitterfeed.com&quot; rel=&quot;nofollow&quot;&gt;twitterfeed&lt;/a&gt;",
                "text": "base16-bytestring 0.1.0.0, added by BryanOSullivan: Fast base16 (hex) encoding and deconding for ByteStrings http://bit.ly/eu75TN",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Tue, 25 Jan 2011 22:12:07 +0000",
                "from_user": "benreads",
                "from_user_id": 199750997,
                "from_user_id_str": "199750997",
                "geo": null,
                "id": 30025146991382528,
                "id_str": "30025146991382528",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a3.twimg.com/sticky/default_profile_images/default_profile_2_normal.png",
                "source": "&lt;a href=&quot;http://twitter.com/&quot;&gt;web&lt;/a&gt;",
                "text": "Writing Systems Software in a Functional Language -- brief, but fun. I want to see how well they can make Systems Haskell perform at scale.",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Tue, 25 Jan 2011 22:09:45 +0000",
                "from_user": "jcawthorne1",
                "from_user_id": 35359797,
                "from_user_id_str": "35359797",
                "geo": null,
                "id": 30024549265309696,
                "id_str": "30024549265309696",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a1.twimg.com/profile_images/1173453785/48893_521140819_1896062_q_normal.jpg",
                "source": "&lt;a href=&quot;http://twitter.com/&quot; rel=&quot;nofollow&quot;&gt;Twitter for iPhone&lt;/a&gt;",
                "text": "Just saw Jeff Haskell give the middle finger!! Lol awesome",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Tue, 25 Jan 2011 22:06:44 +0000",
                "from_user": "goodfox",
                "from_user_id": 13540930,
                "from_user_id_str": "13540930",
                "geo": null,
                "id": 30023790381498369,
                "id_str": "30023790381498369",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a1.twimg.com/profile_images/1205312219/23467_350321383101_821143101_5114147_3010041_n_normal.jpg",
                "source": "&lt;a href=&quot;http://foursquare.com&quot; rel=&quot;nofollow&quot;&gt;foursquare&lt;/a&gt;",
                "text": "At the Haskell Convocation. (@ Haskell Indian Nations U Auditorium) http://4sq.com/eKKXyn",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Tue, 25 Jan 2011 22:02:31 +0000",
                "from_user": "sanityinc",
                "from_user_id": 14674418,
                "from_user_id_str": "14674418",
                "geo": null,
                "id": 30022731919523840,
                "id_str": "30022731919523840",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a2.twimg.com/profile_images/238230125/IMG_1030-1_normal.jpg",
                "source": "&lt;a href=&quot;http://www.echofon.com/&quot; rel=&quot;nofollow&quot;&gt;Echofon&lt;/a&gt;",
                "text": "@xshay Haskell's great, but Clojure's similarly lazy in all the ways that matter, and is more practical for day-to-day use.",
                "to_user": "xshay",
                "to_user_id": 371289,
                "to_user_id_str": "371289"
            },
            {
                "created_at": "Tue, 25 Jan 2011 21:58:29 +0000",
                "from_user": "xshay",
                "from_user_id": 371289,
                "from_user_id_str": "371289",
                "geo": null,
                "id": 30021714444292096,
                "id_str": "30021714444292096",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a2.twimg.com/profile_images/1113482439/me-brisbane_normal.jpg",
                "source": "&lt;a href=&quot;http://itunes.apple.com/us/app/twitter/id409789998?mt=12&quot; rel=&quot;nofollow&quot;&gt;Twitter for Mac&lt;/a&gt;",
                "text": "Just made a lazy sequence of prime numbers in haskell. I'm smitten.",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Tue, 25 Jan 2011 21:48:40 +0000",
                "from_user": "stackfeed",
                "from_user_id": 199453873,
                "from_user_id_str": "199453873",
                "geo": null,
                "id": 30019243982454784,
                "id_str": "30019243982454784",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a3.twimg.com/a/1294785484/images/default_profile_3_normal.png",
                "source": "&lt;a href=&quot;http://twitterfeed.com&quot; rel=&quot;nofollow&quot;&gt;twitterfeed&lt;/a&gt;",
                "text": "Practical Scala reference manual, for searching things like method names: Hello, \n\nInspired by\nHaskell API Searc... http://bit.ly/g4zWZQ",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Tue, 25 Jan 2011 21:45:32 +0000",
                "from_user": "aapnoot",
                "from_user_id": 17489366,
                "from_user_id_str": "17489366",
                "geo": null,
                "id": 30018455189065728,
                "id_str": "30018455189065728",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a3.twimg.com/sticky/default_profile_images/default_profile_2_normal.png",
                "source": "&lt;a href=&quot;http://twitterfeed.com&quot; rel=&quot;nofollow&quot;&gt;twitterfeed&lt;/a&gt;",
                "text": "download 0.3.1, added by MagnusTherning: High-level file download based on URLs http://bit.ly/i8mYvi",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Tue, 25 Jan 2011 21:45:31 +0000",
                "from_user": "aapnoot",
                "from_user_id": 17489366,
                "from_user_id_str": "17489366",
                "geo": null,
                "id": 30018452601180160,
                "id_str": "30018452601180160",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a3.twimg.com/sticky/default_profile_images/default_profile_2_normal.png",
                "source": "&lt;a href=&quot;http://twitterfeed.com&quot; rel=&quot;nofollow&quot;&gt;twitterfeed&lt;/a&gt;",
                "text": "download 0.3.1.1, added by MagnusTherning: High-level file download based on URLs http://bit.ly/eHfiJB",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Tue, 25 Jan 2011 21:45:31 +0000",
                "from_user": "Hackage",
                "from_user_id": 17671137,
                "from_user_id_str": "17671137",
                "geo": null,
                "id": 30018451867176960,
                "id_str": "30018451867176960",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a2.twimg.com/profile_images/290264834/Haskell-logo-outer-glow_normal.png",
                "source": "&lt;a href=&quot;http://twitterfeed.com&quot; rel=&quot;nofollow&quot;&gt;twitterfeed&lt;/a&gt;",
                "text": "download 0.3.1, added by MagnusTherning: High-level file download based on URLs http://bit.ly/i8mYvi",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Tue, 25 Jan 2011 21:45:04 +0000",
                "from_user": "newsyc100",
                "from_user_id": 138502705,
                "from_user_id_str": "138502705",
                "geo": null,
                "id": 30018340839755777,
                "id_str": "30018340839755777",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a1.twimg.com/profile_images/1083845614/yclogo_normal.gif",
                "source": "&lt;a href=&quot;http://news.ycombinator.com&quot; rel=&quot;nofollow&quot;&gt;newsyc&lt;/a&gt;",
                "text": "Haskell improves log processing 4x over Python http://devblog.bu.mp/haskell-at-bump (http://bit.ly/gQxxR8)",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Tue, 25 Jan 2011 21:43:32 +0000",
                "from_user": "magthe",
                "from_user_id": 1578246,
                "from_user_id_str": "1578246",
                "geo": null,
                "id": 30017953801969665,
                "id_str": "30017953801969665",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a0.twimg.com/profile_images/59312315/avatar_simpson_small_normal.jpg",
                "source": "&lt;a href=&quot;http://api.supertweet.net&quot; rel=&quot;nofollow&quot;&gt;MyAuth API Proxy&lt;/a&gt;",
                "text": "New version of download uploaded to #hackage #haskell",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Tue, 25 Jan 2011 21:37:51 +0000",
                "from_user": "reward999",
                "from_user_id": 135838970,
                "from_user_id_str": "135838970",
                "geo": null,
                "id": 30016525180084224,
                "id_str": "30016525180084224",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a2.twimg.com/profile_images/1079929891/logo_normal.png",
                "source": "&lt;a href=&quot;http://twitterfeed.com&quot; rel=&quot;nofollow&quot;&gt;twitterfeed&lt;/a&gt;",
                "text": "Found Great Dane (Main &amp; Haskell- Dallas): We found a great dane. 214-712-0000 http://bit.ly/fQ8iBw",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Tue, 25 Jan 2011 21:34:22 +0000",
                "from_user": "WeiMatas",
                "from_user_id": 137719265,
                "from_user_id_str": "137719265",
                "geo": null,
                "id": 30015646020411392,
                "id_str": "30015646020411392",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a2.twimg.com/profile_images/1092224079/8__4__normal.jpg",
                "source": "&lt;a href=&quot;http://dlvr.it&quot; rel=&quot;nofollow&quot;&gt;dlvr.it&lt;/a&gt;",
                "text": "Fourth day Rez party Eddie Haskell County in second life \u2013 which took place in cabaret Tadd",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Tue, 25 Jan 2011 21:33:21 +0000",
                "from_user": "listwarenet",
                "from_user_id": 144546280,
                "from_user_id_str": "144546280",
                "geo": null,
                "id": 30015392571195392,
                "id_str": "30015392571195392",
                "iso_language_code": "no",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a1.twimg.com/a/1295051201/images/default_profile_1_normal.png",
                "source": "&lt;a href=&quot;http://1e10.org/cloud/&quot; rel=&quot;nofollow&quot;&gt;1e10&lt;/a&gt;",
                "text": "http://www.listware.net/201101/haskell-cafe/83889-haskell-cafe-gpl-license-of-h-matrix-and-prelude-numeric.html Haskell-cafe -",
                "to_user_id": null,
                "to_user_id_str": null
            }
        ],
        "results_per_page": 100,
        "since_id": 0,
        "since_id_str": "0"
    }
  |]

