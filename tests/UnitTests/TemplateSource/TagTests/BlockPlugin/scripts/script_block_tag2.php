<?php
function default_script_block_tag2($params, $content, $template, &$repeat)
{
    if (isset($content)) {
        return 'scriptblock ' . $content;
    }
}
