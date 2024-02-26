<?php

// This takes the *.md files in changelog/ dir and inserts them
// right below the '## [Unreleased]' marker in CHANGELOG.md

$path_to_main_changelog = 'CHANGELOG.md';
$marker = '## [Unreleased]';
$changelog_files_pattern = 'changelog/*.md';
$new_version = $argv[1];

$file_contents = file_get_contents($path_to_main_changelog);

foreach (glob($changelog_files_pattern) as $filename) {
    $content_to_insert = file_get_contents($filename);

    if (!endsWithNewline($content_to_insert)) {
        $content_to_insert .= "\n";
    }

    $file_contents = str_replace(
        $marker,
        $marker . $content_to_insert,
        $file_contents
    );
    unlink($filename);
}


// add the version number and date
$file_contents = str_replace(
    $marker,
    $marker . sprintf("\n\n## [%s] - %s\n", $new_version, date('Y-m-d')),
    $file_contents
);

file_put_contents($path_to_main_changelog, $file_contents);


function endsWithNewline($str): bool
{
    return preg_match('/[\r\n]$/', $str) === 1;
}