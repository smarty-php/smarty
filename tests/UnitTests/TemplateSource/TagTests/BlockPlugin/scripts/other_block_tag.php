<?php
function default_block_tag($params, $content, $template, &$repeat)
{
    if (isset($content)) {
        return 'defaultblock ' . $content;
    }
}
