<?php
/**
 * Smarty PHPunit tests compiler errors
 *

 * @author  Uwe Tews
 */

/**
 * class for compiler tests
 *
 * 
 * @preserveGlobalState    disabled
 *
 */
class ExtendsIssue419Test extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    public function testextends419()
    {
        $this->smarty->setLeftDelimiter('{{');
        $this->smarty->setRightDelimiter('}}');
        $this->assertEquals('child', $this->smarty->fetch('extends:001_parent.tpl|001_child.tpl'));
    }

    public function testextendsSecurity()
    {
        $this->expectException(\Smarty\Exception::class);
        $this->expectExceptionMessageMatches('/Unable to load.*/');
        $this->assertEquals('child', $this->smarty->fetch('string:{include "001_parent.tpl\', var_dump(shell_exec(\'ls\')), 1, 2, 3);}}?>"}'));
    }

}
