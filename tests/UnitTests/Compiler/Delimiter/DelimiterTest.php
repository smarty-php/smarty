<?php
/**
 * Smarty PHPunit tests of delimiter
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for delimiter tests
 *
 * @backupStaticAttributes enabled
 */
class DelimiterTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(__DIR__);
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    /**
     * test <{ }> delimiter
     */
    public function testDelimiter1()
    {
        $this->smarty->left_delimiter = '<{';
        $this->smarty->right_delimiter = '}>';
        $tpl = $this->smarty->createTemplate('eval:<{* comment *}><{if true}><{"hello world"}><{/if}>');
        $this->assertEquals("hello world", $this->smarty->fetch($tpl));
    }

    /**
     * test <-{ }-> delimiter
     */
    public function testDelimiter2()
    {
        $this->smarty->left_delimiter = '<-{';
        $this->smarty->right_delimiter = '}->';
        $tpl = $this->smarty->createTemplate('eval:<-{* comment *}-><-{if true}-><-{"hello world"}-><-{/if}->');
        $this->assertEquals("hello world", $this->smarty->fetch($tpl));
    }

    /**
     * test <--{ }--> delimiter
     */
    public function testDelimiter3()
    {
        $this->smarty->left_delimiter = '<--{';
        $this->smarty->right_delimiter = '}-->';
        $tpl = $this->smarty->createTemplate('eval:<--{* comment *}--><--{if true}--><--{"hello world"}--><--{/if}-->');
        $this->assertEquals("hello world", $this->smarty->fetch($tpl));
    }

    /**
     * test {{ }} delimiter
     */
    public function testDelimiter4()
    {
        $this->smarty->left_delimiter = '{{';
        $this->smarty->right_delimiter = '}}';
        $tpl = $this->smarty->createTemplate('eval:{{* comment *}}{{if true}}{{"hello world"}}{{/if}}');
        $this->assertEquals("hello world", $this->smarty->fetch($tpl));
    }

}
