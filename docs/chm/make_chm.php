<?php

/* 
 PLEASE DO NOT MAKE ANY MAJOR MODIFICATIONS TO THIS CODE!
 There is a new script collection on the way to replace
 these scripts. Please see the htmlhelp folder for the new
 build system.
 
 See make_chm.README for information about this system.
*/

include_once('common.php');
include_once('chm_settings.php');

$INDEX_IN_HTML = "index.html";

if (empty($FANCY_PATH)) { $FANCY_PATH = $HTML_PATH; }

// Files on the top level of the TOC
$MAIN_FILES = array(
    'getting.started.html',
    'smarty.for.designers.html',
    'smarty.for.programmers.html',
    'appendixes.html'
);

// Header for index and toc 
$HEADER = '<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML//EN">
<html>
<head>
  <meta name="generator" content="PHP 4 - Auto TOC script">
  <!-- Sitemap 1.0 -->
</head>
<body>
  <object type="text/site properties">
    <param name="Window Styles" value="0x800227">
  </object>
  <ul>';

makeProjectFile();
makeContentFiles();

// Generate the HTML Help content files 
function makeContentFiles()
{
    global $LANGUAGE, $MANUAL_TITLE, $HEADER, $MAIN_FILES,
           $HTML_PATH, $INDEX_IN_HTML, $FIRST_PAGE;

    $toc   = fopen("chm/smarty_manual_$LANGUAGE.hhc", "w");
    $index = fopen("chm/smarty_manual_$LANGUAGE.hhk", "w");

    // Write out file headers
    fputs_wrapper($toc,   $HEADER);
    fputs_wrapper($index, $HEADER);

    // Read original index file and drop out newlines
    $indexline = oneLiner("$HTML_PATH/$INDEX_IN_HTML");

    // Print out the objects, autoparsing won't find
    mapAndIndex($MANUAL_TITLE, $FIRST_PAGE, "    ", $toc, $index, 21);

    // There is a fancy index
    if ($FIRST_PAGE != $INDEX_IN_HTML) {

        // Find the name of the Table of Contents
        preg_match('|CLASS=\"TOC\" *><DL *><DT *><B *>(.*)</B|U', $indexline, $match);
        if (empty($match[1])) { // Fallback
            $match[1] = "Table of Contents";
        }
        mapAndIndex($match[1], $INDEX_IN_HTML, "    ", $toc, $index, 21);

    }

    // Find the name of the Preface
    preg_match('|<A +HREF="preface.html" *>([^<]*)</A *>|U', $indexline, $match);
    if (empty($match[1])) { // Fallback
        $match[1] = "Preface";
    }
    mapAndIndex($match[1], "preface.html", "    ", $toc, $index);

    // Now autofind the main pages
    $MAIN_REGEXP = join("|", $MAIN_FILES);

    preg_match_all("![IVX]+\. <A\\s+HREF=\"($MAIN_REGEXP)\"\\s*>([^<]+)</A\\s*>.*</DT\\s*></DL\\s*>(?:</DD\\s*></DL\\s*>)?(?:</DD\\s*><DT\\s*>[IVX]|</DIV)!Ui", $indexline, $matches, PREG_SET_ORDER);

    // Go through the main files, and link in subpages
    foreach ($matches as $matchinfo) {
        mapAndIndex($matchinfo[2], $matchinfo[1], "    ", $toc, $index);

        fputs_wrapper($toc, "\n      <ul>\n");
        preg_match_all('!\d+\. <A\\s+HREF="([^"]+)"\\s*>([^<]*)</A\\s*>!iSU', $matchinfo[0], $subpages, PREG_SET_ORDER);

        foreach ($subpages as $spinfo) {
            mapAndIndex($spinfo[2], $spinfo[1], "        ", $toc, $index);
            findDeeperLinks($spinfo[1], $toc, $index);
        }
        fputs_wrapper($toc, "\n      </ul>\n");
    }

    // Write out closing line, and end files
    fputs_wrapper($index, "  </ul>\n</body>\n</html>");
    fputs_wrapper($toc,   "  </ul>\n</body>\n</html>");
    fclose($index);
    fclose($toc);
} // makeContentfiles() function end

// Generates the HTML Help project file
function makeProjectFile()
{
    global $LANGUAGE, $MANUAL_TITLE, $LANGUAGES,
           $HTML_PATH, $FANCY_PATH, $INDEX_IN_HTML,
           $FIRST_PAGE;

    // Try to find the fancy index file
    if (file_exists("$FANCY_PATH/fancy-index.html")) {
        $FIRST_PAGE = 'fancy-index.html';
    } else {
        $FIRST_PAGE = $INDEX_IN_HTML;
    }

    $FIRST_PAGEP = substr($FANCY_PATH, 4) . "\\$FIRST_PAGE";

    // Start writing the project file
    $project = fopen("chm/smarty_manual_$LANGUAGE.hhp", "w");
    fputs_wrapper($project, "[OPTIONS]\n");
    fputs_wrapper($project, "Compatibility=1.1 or later\n");
    fputs_wrapper($project, "Compiled file=smarty_manual_$LANGUAGE.chm\n");
    fputs_wrapper($project, "Contents file=smarty_manual_$LANGUAGE.hhc\n");
    fputs_wrapper($project, "Index file=smarty_manual_$LANGUAGE.hhk\n");
    fputs_wrapper($project, "Default Window=smarty\n");
    fputs_wrapper($project, "Default topic=$FIRST_PAGEP\n");
    fputs_wrapper($project, "Display compile progress=Yes\n");
    fputs_wrapper($project, "Full-text search=Yes\n");

    // Get the proper language code from the array
    fputs_wrapper($project, "Language={$LANGUAGES[$LANGUAGE]["langcode"]}\n");

    // Now try to find out how the manual named in the actual language
    // this must be in the index.html file as the title (DSSSL generated)
    $content = oneLiner("$HTML_PATH/$INDEX_IN_HTML");
    if (preg_match("|<TITLE\s*>([^<]*)</TITLE\s*>|U", $content, $found)) {
        $MANUAL_TITLE = $found[1];
    } else { // Fallback
        $MANUAL_TITLE = "Smarty Manual";
    }

    fputs_wrapper($project, "Title=$MANUAL_TITLE\n");
    fputs_wrapper($project, "Default Font={$LANGUAGES[$LANGUAGE]['preferred_font']}\n");

    // Define the phpdoc window style (adds more functionality)
    fputs_wrapper($project, "\n[WINDOWS]\nsmarty=\"$MANUAL_TITLE\",\"smarty_manual_$LANGUAGE.hhc\",\"smarty_manual_$LANGUAGE.hhk\"," .
          "\"$FIRST_PAGEP\",\"$FIRST_PAGEP\",,,,,0x23520,,0x386e,,,,,,,,0\n");

    // Write out all the filenames as in FANCY_PATH
    fputs_wrapper($project, "\n[FILES]\n");
    $handle = opendir($FANCY_PATH);
    while (false !== ($file = readdir($handle))) {
        if ($file != "." && $file != "..") {
            fputs_wrapper($project, substr($FANCY_PATH, 4)."\\$file\n");
        }
    }
    closedir($handle);
    fclose($project);
} // makeProjectFile() function end

// Print out a SiteMap object for a file
function mapAndIndex($name, $local, $tabs, $toc, $index, $imgnum = "auto")
{
    global $FANCY_PATH;
    $name = str_replace('"', '&quot;', $name);

    fputs_wrapper($toc, "
$tabs<li><object type=\"text/sitemap\">
$tabs  <param name=\"Name\" value=\"$name\">
$tabs  <param name=\"Local\" value=\"".substr($FANCY_PATH, 4)."\\$local\">
");

    if ($imgnum != "auto") {
        fputs_wrapper($toc, "$tabs  <param name=\"ImageNumber\" value=\"$imgnum\">\r\n");
    }
    fputs_wrapper($toc, "$tabs  </object>\r\n");

    fputs_wrapper($index, "
    <li><object type=\"text/sitemap\">
      <param name=\"Local\" value=\"".substr($FANCY_PATH, 4)."\\$local\">
      <param name=\"Name\" value=\"$name\">
    </object></li>
");

} // mapAndIndex() function end


// Process a file, and find any links need to be presented in tree
function findDeeperLinks ($filename, $toc, $index)
{
    global $HTML_PATH;
    $contents = oneLiner("$HTML_PATH/$filename");

    // Find all sublinks
    if (preg_match_all("!<DT\\s*><A\\s+HREF=\"(([\\w\\.-]+\\.)+html)(\\#[\\w\\.-]+)?\"\\s*>([^<]*)</A\\s*>!U", $contents, $matches, PREG_SET_ORDER)) {

        // Print out the file informations for all the links
        fputs_wrapper($toc, "\n        <ul>");
        foreach ($matches as $onematch) {
            $param["html"] = $onematch[1];
            if (!empty($onematch[3])) {
                $param["html"] .= $onematch[3];
            }
            $param["title"] = strip_tags($onematch[4]);
            mapAndIndex($param["title"], $param["html"], "          ", $toc, $index);
        }
        fputs_wrapper($toc, "        </ul>\n");

    } else {
        echo "no deeper TOC info found in $filename\n";
    }
    
} // findDeeperLinks() function end
?>
