<?php
// Used directories and files
$HTML_PATH     = getenv("PHP_HELP_COMPILE_DIR");
$FANCY_PATH    = getenv("PHP_HELP_COMPILE_FANCYDIR");
$LANGUAGE      = getenv("PHP_HELP_COMPILE_LANG");
$INTERNAL_CHARSET = "UTF-8";

// Return a file joined on one line
function oneLiner($filename, $only_tags = false)
{
    global $INTERNAL_CHARSET;

    if ($only_tags) {
        $buf = @preg_replace("/<([a-zA-Z1-9]+)(>|[^a-zA-Z1-9][^>]*>)/Ue", "preg_replace('/[\r\n]{1,2}/U', ' ', \"<\$1 \$2\")", file_get_contents($filename));
    } else {
        $buf = preg_replace("/[\r|\n]{1,2}/U", " ", file_get_contents($filename));
    }
    $charset = detectDocumentCharset($buf);

    if ($charset === false) $charset = "UTF-8";

    if ($charset != $INTERNAL_CHARSET) {
        if (function_exists("iconv")) {
            $buf = iconv($charset, $INTERNAL_CHARSET, $buf);
        } elseif (function_exists("mb_convert_encoding")) {
            $buf = mb_convert_encoding($buf, $INTERNAL_CHARSET, $charset);
        } elseif (preg_match("/^UTF-?8$/i", $INTERNAL_CHARSET) && preg_match("/^(ISO-8859-1|WINDOWS-1252)$/i", $charset)) {
            $buf = utf8_encode($buf);
        } else {
            die("charset conversion function is not available.");
        }
    }
    return $buf;
}

function fputs_wrapper($fp, $str)
{
    fputs($fp, convertCharset($str));
}

function convertCharset($buf)
{
    global $LANGUAGE, $LANGUAGES, $INTERNAL_CHARSET;

    $charset = $LANGUAGES[$LANGUAGE]['preferred_charset'];

    if ($charset != $INTERNAL_CHARSET) {
        if (function_exists("iconv")) {
            $buf = iconv($INTERNAL_CHARSET, "$charset//TRANSLIT", $buf);
        } elseif (function_exists("mb_convert_encoding")) {
            $buf = mb_convert_encoding($buf, $charset, $INTERNAL_CHARSET);
        } elseif (preg_match("/^UTF-?8$/i", $INTERNAL_CHARSET) && preg_match("/^(ISO-8859-1|WINDOWS-1252)$/i", $charset)) {
            $buf = utf8_decode($buf);
        } else {
            die("$LANGUAGE locale is not supported.");
        }
    }
    return $buf;
} // oneLiner() function end

// Returns the name of character set in the given document
function detectDocumentCharset($doc)
{
    if (preg_match('/<META[^>]+CHARSET=["\'\s]?([\w\d-]+)["\'\s]?\s*>/iS', $doc, $reg)) {
        return $reg[1];
    }
    return false;
}

function setDocumentCharset($doc, $charset)
{
    return preg_replace("/(<META\\s+HTTP-EQUIV=\"CONTENT-TYPE\"\\s+CONTENT=\"TEXT\\/HTML;\\s+CHARSET=)([\\w\\d-]*)(\"\\s*>)/iU", "\$1$charset\$3", $doc);
}
?>

