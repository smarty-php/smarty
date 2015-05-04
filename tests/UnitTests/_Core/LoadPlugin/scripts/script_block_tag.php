<?php
function default_script_block_tag($params, $content, $template, &$repeat)
{
    if (isset($content)) {
        return 'scriptblock ' . $content;
    }
}
