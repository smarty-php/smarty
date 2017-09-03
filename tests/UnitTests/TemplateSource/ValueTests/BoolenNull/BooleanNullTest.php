<?php
/**
 * Smarty PHPunit tests literals true false null
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for {$smarty.ldelim} {$smarty.rdelim} tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class BooleanNullTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
    }

    public function testInit()
    {
        $this->cleanDirs();
    }
    /**
     * test true
     *
     */
    public function testTrue() {
        $this->smarty->assign('value', true);
        $this->assertEquals('true', $this->smarty->fetch('eval:{if $value === true}true{else}false{/if}'));
    }
    /**
     * test false
     *
     */
    public function testFalse() {
        $this->smarty->assign('value', false);
        $this->assertEquals('true', $this->smarty->fetch('eval:{if $value === false}true{else}false{/if}'));
    }
    /**
     * test null
     *
     */
    public function testNull() {
        $this->smarty->assign('value', null);
        $this->assertEquals('true', $this->smarty->fetch('eval:{if $value === null}true{else}false{/if}'));
    }
 }
