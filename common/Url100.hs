{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Url100
  ( encodedUrl100
  , byteStringUrl100
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

encodedUrl100 :: ByteArray
encodedUrl100 =
  shortByteStringToByteArray (toShort byteStringUrl100)

byteStringUrl100 :: ByteString
byteStringUrl100 = encodeUtf8
  [text|
    [ "https://balance.example.com/aunt.htm"
    , "http://anger.example.com/"
    , "https://www.example.edu/basket/branch#boat"
    , "http://www.example.com/bag?bead=approval"
    , "https://example.com/bike.php"
    , "https://www.example.net/"
    , "https://www.example.com/"
    , "https://example.com/birds"
    , "https://aftermath.example.com/"
    , "http://badge.example.org/bee/brake"
    , "https://bedroom.example.com/"
    , "http://www.example.com/activity.aspx"
    , "http://example.net/book/anger.aspx?aunt=base&bone=amusement"
    , "http://example.net/art/apparatus.php"
    , "http://www.example.net/boat/brick.html"
    , "http://www.example.com/amount.aspx#bat"
    , "http://ants.example.net/blow/bubble"
    , "http://example.com/badge.php"
    , "http://www.example.com/afterthought.php"
    , "http://www.example.com/"
    , "https://bit.example.com/"
    , "https://adjustment.example.com/?bee=beginner"
    , "http://example.com/bed/ball.html"
    , "http://www.example.net/"
    , "https://example.com/bikes/actor.php"
    , "https://www.example.com/brick"
    , "http://example.com/"
    , "http://example.com/branch/brake.html?afterthought=bee"
    , "http://www.example.net/?bells=blood&art=back"
    , "https://www.example.com/"
    , "http://achiever.example.com/arch?bit=boat"
    , "https://example.net/?believe=amusement&bee=book"
    , "http://www.example.com/bell"
    , "http://www.example.com/brake/bath"
    , "http://example.edu/#bikes"
    , "https://example.com/badge/birthday"
    , "https://www.example.com/art/afterthought"
    , "http://www.example.com/boundary.php"
    , "https://www.example.com/art"
    , "https://www.example.org/basket"
    , "https://www.example.com/blade.html?attraction=basket"
    , "http://www.example.com/balance.php"
    , "https://bike.example.com/advertisement"
    , "https://bag.example.org/art/attack.php?base=birds&authority=act"
    , "https://example.com/basin.php"
    , "https://example.com/babies/beginner.htm"
    , "https://www.example.org/"
    , "http://bike.example.com/"
    , "https://www.example.com/"
    , "http://www.example.com/"
    , "https://example.com/"
    , "https://www.example.com/alarm/air?account=account"
    , "https://example.com/"
    , "https://example.net/#ants"
    , "http://www.example.com/adjustment/boot"
    , "http://example.org/branch.htm"
    , "https://www.example.net/battle?boundary=airplane"
    , "http://example.com/"
    , "https://example.net/?book=board#ball"
    , "http://aftermath.example.com/#boot"
    , "https://www.example.org/"
    , "http://example.com/"
    , "http://example.com/alarm.html"
    , "http://example.com/bait"
    , "http://example.org/berry.php#alarm"
    , "http://www.example.net/baby/addition"
    , "http://www.example.org/"
    , "http://www.example.org/"
    , "https://example.com/"
    , "https://example.com/branch/adjustment"
    , "https://www.example.net/airport.aspx"
    , "http://www.example.com/alarm"
    , "https://www.example.com/afternoon"
    , "https://example.org/?brick=airplane#appliance"
    , "http://example.com/#bell"
    , "http://arithmetic.example.net/activity/beginner"
    , "http://beds.example.com/"
    , "https://art.example.com/"
    , "https://www.example.com/"
    , "https://www.example.com/alarm.html?beds=bottle"
    , "http://www.example.com/bike/baseball.html"
    , "http://www.example.com/"
    , "https://example.com/?blade=base"
    , "https://example.com/"
    , "https://example.org/bubble/amusement"
    , "http://example.com/ball/basket?activity=boat&branch=books"
    , "http://www.example.org/"
    , "https://www.example.com/army"
    , "http://bubble.example.com/#basketball"
    , "https://example.com/"
    , "https://www.example.com/box/basketball.php"
    , "http://agreement.example.com/breath.html#bag"
    , "http://www.example.com/"
    , "https://www.example.com/"
    , "http://example.com/?act=addition&bee=action"
    , "https://www.example.com/back"
    , "https://www.example.org/?argument=boundary"
    , "http://example.org/"
    , "http://example.org/anger.htm"
    , "http://attack.example.org/"
    ]
  |]


