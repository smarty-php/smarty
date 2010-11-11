<?php

/* 
PLEASE DO NOT MAKE ANY MAJOR MODIFICATIONS TO THIS CODE!
There is a new script collection on the way to replace
these scripts. Please be patient while it will be ready
to put here in CVS.
*/

include_once('common.php');
include_once('chm_settings.php');

// This script takes much time to run
set_time_limit(0);

// Get ENV vars from the system
$original_index = "index.html";

// How many files were processed
$counter = 0;

// Open the directory, and do the work on all HTML files
$handle = opendir($HTML_PATH);
while (false !== ($filename = readdir($handle))) {
    if (strpos($filename, ".html") && ($filename != "fancy-index.html")) {
        fancy_design($filename);
    }
}
closedir($handle);

// Look for CHM index file (snap-downloader, cvs-usr with/without lang-support) 
if (false == ($content = oneLiner("$LANGUAGE/make_chm_index.html", true))) {
    $content = oneLiner("en/make_chm_index.html", true);
}

// Make GENTIME the actual date/time
$content = str_replace("[GENTIME]", date("D M d H:i:s Y"), $content);
$content = str_replace("[PUBTIME]", $publication_date, $content);
$content = setDocumentCharset($content, $LANGUAGES[$LANGUAGE]['mime_charset_name']);
$fp = fopen("$FANCY_PATH/fancy-index.html", "w");
fputs_wrapper($fp, $content);
fclose($fp);

copy("chm/make_chm_style.css", "$FANCY_PATH/style.css");
copy("chm/make_chm_spc.gif", "$FANCY_PATH/spacer.gif");

// Three files added (fancy-index.html, style.css and spacer.gif)
$counter += 3;
  
echo "\nConverting ready...\n";
echo "Total number of files written in $FANCY_PATH directory: $counter\n\n";
  
/***********************************************************************/
/* End of script lines, one main function follows                      */
/***********************************************************************/

// Convert one file from HTML => fancy HTML
function fancy_design($fname)
{
    global $HTML_PATH, $FANCY_PATH, $LANGUAGE, $LANGUAGES, $counter, $original_index, $publication_date;

    // Get the contents of the file from $HTML_PATH
    $content = oneLiner("$HTML_PATH/$fname", true);

    // CSS file linking
    $content = preg_replace("|</HEAD|", '<LINK REL="stylesheet" HREF="style.css"></HEAD', $content);

    // No margins around
    $content = preg_replace("/<BODY/", '<BODY TOPMARGIN="0" LEFTMARGIN="0"', $content);

    // HR dropout
    $content = preg_replace("/<HR\\s+ALIGN=\"LEFT\"\\s+WIDTH=\"100%\">/", '', $content);

    // Whole page table and backgrounds
    $wpbegin = '<TABLE BORDER="0" WIDTH="100%" HEIGHT="100%" CELLSPACING="0" CELLPADDING="0"><TR><TD COLSPAN="3">';
    $bnavt = '<TABLE BGCOLOR="#CCCCFF" BORDER="0" CELLPADDING="0" CELLSPACING="0" WIDTH="100%">';
    $lnavt = '<TR BGCOLOR="#333366"><TD><IMG SRC="spacer.gif" BORDER="0" WIDTH="1" HEIGHT="1"><BR></TD></TR>';
    $space = '<IMG SRC="spacer.gif" WIDTH="10" HEIGHT="1">';

    // Navheader backgound
    $content = preg_replace("/<DIV\\s+CLASS=\"NAVHEADER\"\\s*><TABLE(.*)CELLPADDING=\"0\"(.*)<\\/TABLE\\s*><\\/DIV\\s*>/Us",
        $wpbegin . '<DIV CLASS="NAVHEADER">' . $bnavt . '<TR><TD><TABLE\\1CELLPADDING="3"\\2</TABLE></TD></TR>' . $lnavt . '</TABLE></DIV></TD></TR><TR><TD>' . $space . '</TD><TD HEIGHT="100%" VALIGN="TOP" WIDTH="100%"><BR>', $content);

    // Navfooter backgound
    $content = preg_replace("/<DIV\\s+CLASS=\"NAVFOOTER\"\\s*><TABLE(.*)CELLPADDING=\"0\"(.*)<\\/TABLE\\s*><\\/DIV\\s*>/Us",
        '<BR></TD><TD>' . $space . '</TD></TR><TR><TD COLSPAN="3"><DIV CLASS="NAVFOOTER">' . $bnavt . $lnavt . '<TR><TD><TABLE\\1CELLPADDING="3"\\2</TABLE></TD></TR></TABLE></DIV></TD></TR></TABLE>', $content);

    // Fix copyright page fault...
    if ($fname == "copyright.html") {
        $content = preg_replace("/&#38;copy;/", "&copy;", $content);
        $content = preg_replace("/<A\\s+HREF=\"$original_index#(authors|translators)\"/U", "<A HREF=\"fancy-index.html\"", $content);
        $content = preg_replace("|(</TH\\s*></TR\\s*>)|", "\\1<TR><TH COLSPAN=\"3\" ALIGN=\"center\">&nbsp;</TH></TR>", $content);
        $content = preg_replace("|(&nbsp;</TD\\s*></TR\\s*>)|", "\\1<TR><TD COLSPAN=\"3\" ALIGN=\"center\">&nbsp;</TD></TR>", $content);
    }

    // Fix the original manual index to look far better...
    elseif ($fname == "$original_index") {

        // Find out manual generation date
        if (preg_match('|<P\s+CLASS="pubdate"\s*>([\\d-]+)<BR></P\s*>|U', $content, $match)) {
            $publication_date = $match[1];
        } else { 
            $publication_date = 'n/a';
        }

        // Modify the index file to meet our needs
        preg_match('|CLASS=\"title\"\\s*><A\\s+NAME=\"manual\"\\s*>(.*)</A\\s*>(.*)</H1|U', $content, $match);
        $indexchange = '<TABLE BORDER="0" WIDTH="100%" HEIGHT="100%" CELLSPACING="0" CELLPADDING="0"><TR><TD COLSPAN="3"><DIV CLASS="NAVHEADER"><TABLE BGCOLOR="#CCCCFF" BORDER="0" CELLPADDING="0" CELLSPACING="0" WIDTH="100%"><TR><TD><TABLE
        WIDTH="100%" BORDER="0" CELLPADDING="3" CELLSPACING="0"><TR><TH COLSPAN="3">'.$match[2].'</TH></TR><TR><TD COLSPAN="3" ALIGN="center">&nbsp;</TD></TR></TABLE></TD></TR><TR BGCOLOR="#333366"><TD><IMG SRC="spacer.gif" BORDER="0" WIDTH="1" HEIGHT="1"><BR></TD></TR></TABLE>
        </DIV></TD></TR><TR><TD><IMG SRC="spacer.gif" WIDTH="10" HEIGHT="1"></TD><TD HEIGHT="100%" VALIGN="TOP" WIDTH="100%"><BR>';
        $content = preg_replace("/(<DIV\\s+CLASS=\"BOOK\")/", "$indexchange\\1", $content);
        $content = preg_replace("/(<DIV\\s+CLASS=\"author\").*<HR>/Us", "", $content);
        preg_match('|<DIV\\s+CLASS="TOC"\\s*><DL\\s*><DT\\s*><B\\s*>(.*)</B\\s*>|U', $content, $match);
        $content = preg_replace("|(CLASS=\"title\"\\s+><A\\s+NAME=\"manual\"\\s*>).*(</A\\s*>).*(</H1)|U", "\\1$match[1]\\2\\3", $content);
        $content = preg_replace("|<DT\\s*><B\\s*>(.*)</B\\s*></DT\\s*>|U", "", $content);

    }

    // Print out that new file to $FANCY_PATH
    $fp = fopen("$FANCY_PATH/$fname", "w");
    $content = setDocumentCharset($content, $LANGUAGES[$LANGUAGE]['mime_charset_name']);
    fputs_wrapper($fp, $content);
    fclose($fp);

    // Print out a message to see the progress
    echo "$FANCY_PATH/$fname ready...\n";
    $counter++;
    
} // fancy_design() function end

?>
