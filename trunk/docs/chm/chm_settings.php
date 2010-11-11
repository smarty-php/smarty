<?php
$DEFAULT_FONT = "Arial,10,0";

// Array to manual code -> HTML Help Code conversion
// Code list: http://www.helpware.net/htmlhelp/hh_info.htm
// Charset list: http://www.microsoft.com/globaldev/nlsweb/default.asp
// Language code: http://www.unicode.org/unicode/onlinedat/languages.html
// MIME preferred charset list: http://www.iana.org/assignments/character-sets
// Font list: http://www.microsoft.com/office/ork/xp/three/inte03.htm

$LANGUAGES = array(
    "hk"    => array(
                   "langcode" => "0xc04 Hong Kong Cantonese",
                   "preferred_charset" => "CP950",
                   "mime_charset_name" => "Big5",
                   "preferred_font" => "MingLiu,10,0"
               ),
    "tw"    => array(
                   "langcode" => "0x404 Traditional Chinese",
                   "preferred_charset" => "CP950",
                   "mime_charset_name" => "Big5",
                   "preferred_font" => "MingLiu,10,0"
               ),
    "cs"    => array(
                   "langcode" => "0x405 Czech",
                   "preferred_charset" => "Windows-1250",
                   "mime_charset_name" => "Windows-1250",
                   "preferred_font" => $DEFAULT_FONT,
               ),
    "da"    => array(
                   "langcode" => "0x406 Danish",
                   "preferred_charset" => "Windows-1252",
                   "mime_charset_name" => "Windows-1252",
                   "preferred_font" => $DEFAULT_FONT,
               ),
    "de"    => array(
                   "langcode" => "0x407 German (Germany)",
                   "preferred_charset" => "Windows-1252",
                   "mime_charset_name" => "Windows-1252",
                   "preferred_font" => $DEFAULT_FONT,
               ),
    "el"    => array(
                   "langcode" => "0x408 Greek",
                   "preferred_charset" => "Windows-1253",
                   "mime_charset_name" => "Windows-1253",
                   "preferred_font" => $DEFAULT_FONT
               ),
    "en"    => array(
                   "langcode" => "0x809 English (United Kingdom)",
                   "preferred_charset" => "Windows-1252",
                   "mime_charset_name" => "Windows-1252",
                   "preferred_font" => $DEFAULT_FONT
               ),
    "es"    => array(
                   "langcode" => "0xc0a Spanish (International Sort)",
                   "preferred_charset" => "Windows-1252",
                   "mime_charset_name" => "Windows-1252",
                   "preferred_font" => $DEFAULT_FONT
               ),
    "fr"    => array(
                   "langcode" => "0x40c French (France)",
                   "preferred_charset" => "Windows-1252",
                   "mime_charset_name" => "Windows-1252",
                   "preferred_font" => $DEFAULT_FONT
               ),
    "fi"    => array(
                   "langcode" => "0x40b Finnish",
                   "preferred_charset" => "Windows-1252",
                   "mime_charset_name" => "Windows-1252",
                   "preferred_font" => $DEFAULT_FONT
               ),
    "he"    => array(
                   "langcode" => "0x40d Hebrew",
                   "preferred_charset" => "Windows-1255",
                   "mime_charset_name" => "Windows-1255",
                   "preferred_font" => $DEFAULT_FONT
               ),
    "hu"    => array(
                   "langcode" => "0x40e Hungarian",
                   "preferred_charset" => "Windows-1250",
                   "mime_charset_name" => "Windows-1250",
                   "preferred_font" => $DEFAULT_FONT
               ),
    "it"    => array(
                   "langcode" => "0x410 Italian (Italy)",
                   "preferred_charset" => "Windows-1252",
                   "mime_charset_name" => "Windows-1252",
                   "preferred_font" => $DEFAULT_FONT
               ),
    "ja"    => array(
                   "langcode" => "0x411 Japanese",
                   "preferred_charset" => "CP932",
                   "mime_charset_name" => "csWindows31J",
                   "preferred_font" => "MS PGothic,10,0"
               ),
    "kr"    => array(
                   "langcode" => "0x412 Korean",
                   "preferred_charset" => "CP949",
                   "mime_charset_name" => "EUC-KR",
                   "preferred_font" => "Gulim,10,0"
               ),
    "nl"    => array(
                   "langcode" => "0x413 Dutch (Netherlands)",
                   "preferred_charset" => "Windows-1252",
                   "mime_charset_name" => "Windows-1252",
                   "preferred_font" => $DEFAULT_FONT
               ),
    "pl"    => array(
                   "langcode" => "0x415 Polish",
                   "preferred_charset" => "Windows-1250",
                   "mime_charset_name" => "Windows-1250",
                   "preferred_font" => $DEFAULT_FONT
               ),
    "pt_BR" => array(
                   "langcode" => "0x416 Portuguese (Brazil)",
                   "preferred_charset" => "Windows-1252",
                   "mime_charset_name" => "Windows-1252",
                   "preferred_font" => $DEFAULT_FONT
               ),
    "ro"    => array(
                   "langcode" => "0x418 Romanian",
                   "preferred_charset" => "Windows-1250",
                   "mime_charset_name" => "Windows-1250",
                   "preferred_font" => $DEFAULT_FONT
               ),
    "ru"    => array(
                   "langcode" => "0x419 Russian",
                   "preferred_charset" => "Windows-1251",
                   "mime_charset_name" => "Windows-1251",
                   "preferred_font" => $DEFAULT_FONT
               ),
    "sk"    => array(
                   "langcode" => "0x41b Slovak",
                   "preferred_charset" => "Windows-1250",
                   "mime_charset_name" => "Windows-1250",
                   "preferred_font" => $DEFAULT_FONT
               ),
    "sl"    => array(
                   "langcode" => "0x424 Slovenian",
                   "preferred_charset" => "Windows-1250",
                   "mime_charset_name" => "Windows-1250",
                   "preferred_font" => $DEFAULT_FONT
               ),
    "sv"    => array(
                   "langcode" => "0x41d Swedish",
                   "preferred_charset" => "Windows-1252",
                   "mime_charset_name" => "Windows-1252",
                   "preferred_font" => $DEFAULT_FONT
               ),
    "zh"    => array(
                   "langcode" => "0x804 Simplified Chinese",
                   "preferred_charset" => "CP936",
                   "mime_charset_name" => "gb2312",
                   "preferred_font" => "simsun,10,0"
               )
);
?>
