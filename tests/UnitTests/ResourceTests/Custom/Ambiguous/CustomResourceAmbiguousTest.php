<?php
/**
 * Smarty PHPunit tests for File resources
 *

 * @author  Uwe Tews
 */

/**
 * class for file resource tests
 */
class CustomResourceAmbiguousTest extends PHPUnit_Smarty
{
    public $_resource = null;

    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
        require_once __DIR__ . '/PHPunitplugins/resource.ambiguous.php';

        // empty the template dir
        $this->smarty->setTemplateDir(array());

        // kill cache for unit test
        //        Smarty::$_resource_cache = array();
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    protected function relative($path)
    {
        $path = str_replace(__DIR__, '.', $path);
        if (DIRECTORY_SEPARATOR == "\\") {
            $path = str_replace("\\", "/", $path);
        }

        return $path;
    }

    public function testNone()
    {
        $resource_handler = new Smarty_Resource_AmbiguousPlugin(__DIR__ . '/templates/ambiguous/');
        $this->smarty->registerResource('ambiguous', $resource_handler);
        $this->smarty->setDefaultResourceType('ambiguous');
//        $this->smarty->setAllowAmbiguousResources(true);

        $tpl = $this->smarty->createTemplate('foobar.tpl');
        $this->assertFalse($tpl->getSource()->exists);
    }

    public function testCase1()
    {
        $resource_handler = new Smarty_Resource_AmbiguousPlugin(__DIR__ . '/templates/ambiguous/');
        $this->smarty->registerResource('ambiguous', $resource_handler);
        $this->smarty->setDefaultResourceType('ambiguous');
//        $this->smarty->setAllowAmbiguousResources(true);

        $resource_handler->setSegment('case1');

        $tpl = $this->smarty->createTemplate('foobar.tpl');
        $this->assertTrue($tpl->getSource()->exists);
        $this->assertEquals('case1', $tpl->getSource()->getContent());
    }

    public function testCase2()
    {
        $resource_handler = new Smarty_Resource_AmbiguousPlugin(__DIR__ . '/templates/ambiguous/');
        $this->smarty->registerResource('ambiguous', $resource_handler);
        $this->smarty->setDefaultResourceType('ambiguous');
//        $this->smarty->setAllowAmbiguousResources(true);

        $resource_handler->setSegment('case2');

        $tpl = $this->smarty->createTemplate('foobar.tpl');
        $this->assertTrue($tpl->getSource()->exists);
        $this->assertEquals('case2', $tpl->getSource()->getContent());
    }


    public function testCaseSwitching()
    {
        $resource_handler = new Smarty_Resource_AmbiguousPlugin(__DIR__ . '/templates/ambiguous/');
        $this->smarty->registerResource('ambiguous', $resource_handler);
        $this->smarty->setDefaultResourceType('ambiguous');
//        $this->smarty->setAllowAmbiguousResources(true);

        $resource_handler->setSegment('case1');
        $tpl = $this->smarty->createTemplate('foobar.tpl');
        $this->assertTrue($tpl->getSource()->exists);
        $this->assertEquals('case1', $tpl->getSource()->getContent());

        $resource_handler->setSegment('case2');
        $tpl = $this->smarty->createTemplate('foobar.tpl');
        $this->assertTrue($tpl->getSource()->exists);
        $this->assertEquals('case2', $tpl->getSource()->getContent());
    }
}
