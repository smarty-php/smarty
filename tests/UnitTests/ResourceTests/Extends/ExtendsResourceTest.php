<?php
/**
 * Smarty PHPunit tests for Extendsresource
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for extends resource tests
 *
 * @backupStaticAttributes enabled
 */
class ExtendsResourceTest extends PHPUnit_Smarty
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
     * test  child/parent template chain with prepend
     */
    public function testCompileBlockChildPrepend_003()
    {
        $result = $this->smarty->fetch('extends:003_parent.tpl|003_child_prepend.tpl');
        $this->assertContains("prepend - Default Title", $result);
    }

    /**
     * test  child/parent template chain with apppend
     */
    public function testCompileBlockChildAppend_004()
    {
        $this->smarty->merge_compiled_includes = true;
        $result = $this->smarty->fetch('extends:004_parent.tpl|004_child_append.tpl');
        $this->assertContains("Default Title - append", $result);
    }
}

