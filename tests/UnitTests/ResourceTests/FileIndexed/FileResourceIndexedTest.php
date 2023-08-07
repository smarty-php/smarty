<?php

/**
 * Smarty PHPunit tests for File resources
 *

 * @author                 Rodney Rehm
 * 
 * 
 * 
 */
class FileResourceIndexedTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
        $this->smarty->addTemplateDir(__DIR__ . '/templates_2');
        // note that 10 is a string!
        $this->smarty->addTemplateDir(__DIR__ . '/templates_3', '10');
        $this->smarty->addTemplateDir(__DIR__ . '/templates_4', 'foo');
     }

    public function testInit()
    {
        $this->cleanDirs();
    }

    public function testFetch()
    {
        $tpl = $this->smarty->createTemplate('dirname.tpl');
        $this->assertEquals('templates', $this->smarty->fetch($tpl));
    }

    public function testFetchNumber()
    {
        $tpl = $this->smarty->createTemplate('[1]dirname.tpl');
        $this->assertEquals('templates_2', $this->smarty->fetch($tpl));
    }

    public function testFetchNumeric()
    {
        $tpl = $this->smarty->createTemplate('[10]dirname.tpl');
        $this->assertEquals('templates_3', $this->smarty->fetch($tpl));
    }
    public function testFetchNumeric2()
    {
        $tpl = $this->smarty->createTemplate('[10, 1]dirname10.tpl');
        $this->assertEquals('templates_3', $this->smarty->fetch($tpl));
    }
    public function testFetchNumeric3()
    {
        $tpl = $this->smarty->createTemplate('[10, 1]dirname1.tpl');
        $this->assertEquals('templates_2', $this->smarty->fetch($tpl));
    }

    public function testFetchName()
    {
        $tpl = $this->smarty->createTemplate('[foo]dirname.tpl');
        $this->assertEquals('templates_4', $this->smarty->fetch($tpl));
    }
    public function testFetchName1()
    {
        $tpl = $this->smarty->createTemplate('[10,0,1,foo]dirname_foo.tpl');
        $this->assertEquals('dirname_foo', $this->smarty->fetch($tpl));
    }
    public function testFetchName2()
    {
        $tpl = $this->smarty->createTemplate('[0,1,foo,10]dirname_x.tpl');
        $this->assertEquals('templates_2', $this->smarty->fetch($tpl));
    }
    public function testFetchName3()
    {
        $tpl = $this->smarty->createTemplate('[0,10,foo,1]dirname_x.tpl');
        $this->assertEquals('templates_4', $this->smarty->fetch($tpl));
    }

    public function testGetCompiledFilepath()
    {
        $tpl = $this->smarty->createTemplate('[foo]dirname.tpl');
        $tpl2 = $this->smarty->createTemplate('dirname.tpl');

		$this->assertNotEquals($tpl->getCompiled()->filepath, $tpl2->getCompiled()->filepath);
    }

    public function testGetCachedFilepath()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
	    $tpl = $this->smarty->createTemplate('[foo]dirname.tpl');
	    $tpl2 = $this->smarty->createTemplate('dirname.tpl');

	    $this->assertNotEquals($tpl->getCached()->filepath, $tpl2->getCached()->filepath);
    }
}
