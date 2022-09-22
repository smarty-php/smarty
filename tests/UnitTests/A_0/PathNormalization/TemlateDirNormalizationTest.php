<?php
/*
 * This file is part of the Smarty PHPUnit tests.
 */

/**
 * class for path normalization tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState    disabled
 * @backupStaticAttributes enabled
 */
class TemplateNormalizationTest extends PHPUnit_Smarty
{
    /*
     * Setup test fixture
     */
    public function setUp(): void
    {
        $this->setUpSmarty(dirname(__FILE__));
    }

    public function testGetTemplateDir()
    {
        $result = $this->smarty->getTemplateDir();
        $this->assertEquals(1, count($result));
        $this->assertEquals(getcwd() . DIRECTORY_SEPARATOR . 'templates' . DIRECTORY_SEPARATOR, $result[ 0 ]);
    }

    public function testSetTemplateDir1()
    {
        $result = $this->smarty->getTemplateDir();
        $this->assertEquals(1, count($result));
        $this->smarty->setTemplateDir('foo');
        $result = $this->smarty->getTemplateDir();
        $this->assertEquals(1, count($result));
        $this->assertEquals(getcwd() . DIRECTORY_SEPARATOR . 'foo' . DIRECTORY_SEPARATOR, $result[ 0 ]);
    }

    public function testSetTemplateDir2()
    {
        $result = $this->smarty->getTemplateDir();
        $this->assertEquals(1, count($result));
        $this->smarty->setTemplateDir('foo/');
        $result = $this->smarty->getTemplateDir();
        $this->assertEquals(1, count($result));
        $this->assertEquals(getcwd() . DIRECTORY_SEPARATOR . 'foo' . DIRECTORY_SEPARATOR, $result[ 0 ]);
    }

    public function testSetTemplateDir3()
    {
        $result = $this->smarty->getTemplateDir();
        $this->assertEquals(1, count($result));
        $this->smarty->setTemplateDir('foo/.');
        $result = $this->smarty->getTemplateDir();
        $this->assertEquals(1, count($result));
        $this->assertEquals(getcwd() . DIRECTORY_SEPARATOR . 'foo' . DIRECTORY_SEPARATOR, $result[ 0 ]);
    }

    public function testSetTemplateDir4()
    {
        $result = $this->smarty->getTemplateDir();
        $this->assertEquals(1, count($result));
        $this->smarty->setTemplateDir('foo/./');
        $result = $this->smarty->getTemplateDir();
        $this->assertEquals(1, count($result));
        $this->assertEquals(getcwd() . DIRECTORY_SEPARATOR . 'foo' . DIRECTORY_SEPARATOR, $result[ 0 ]);
    }

    public function testSetTemplateDir5()
    {
        $result = $this->smarty->getTemplateDir();
        $this->assertEquals(1, count($result));
        $this->smarty->setTemplateDir('.foo');
        $result = $this->smarty->getTemplateDir();
        $this->assertEquals(1, count($result));
        $this->assertEquals(getcwd() . DIRECTORY_SEPARATOR . '.foo' . DIRECTORY_SEPARATOR, $result[ 0 ]);
    }

    public function testSetTemplateDir6()
    {
        $result = $this->smarty->getTemplateDir();
        $this->assertEquals(1, count($result));
        $this->smarty->setTemplateDir('..foo');
        $result = $this->smarty->getTemplateDir();
        $this->assertEquals(1, count($result));
        $this->assertEquals(getcwd() . DIRECTORY_SEPARATOR . '..foo' . DIRECTORY_SEPARATOR, $result[ 0 ]);
    }

    public function testSetTemplateDir7()
    {
        $result = $this->smarty->getTemplateDir();
        $this->assertEquals(1, count($result));
        $this->smarty->setTemplateDir('../foo');
        $result = $this->smarty->getTemplateDir();
        $this->assertEquals(1, count($result));
        $this->assertEquals(substr(getcwd(), 0, - strlen(basename(getcwd()))) . 'foo' . DIRECTORY_SEPARATOR,
                            $result[ 0 ]);
    }

    public function testSetTemplateDir8()
    {
        $this->smarty->setTemplateDir(array('foo', 'bah'));
        $result = $this->smarty->getTemplateDir();
        $this->assertEquals(2, count($result));
        $this->assertEquals(getcwd() . DIRECTORY_SEPARATOR . 'foo' . DIRECTORY_SEPARATOR, $result[ 0 ]);
        $this->assertEquals(getcwd() . DIRECTORY_SEPARATOR . 'bah' . DIRECTORY_SEPARATOR, $result[ 1 ]);
    }

    public function testSetTemplateDir9()
    {
        $this->smarty->setTemplateDir(array('foo', 'bah'));
        $result = $this->smarty->getTemplateDir();
        $this->assertEquals(2, count($result));
        $this->smarty->addTemplateDir('blar');
        $result = $this->smarty->getTemplateDir();
        $this->assertEquals(3, count($result));
        $this->assertEquals(getcwd() . DIRECTORY_SEPARATOR . 'foo' . DIRECTORY_SEPARATOR, $result[ 0 ]);
        $this->assertEquals(getcwd() . DIRECTORY_SEPARATOR . 'bah' . DIRECTORY_SEPARATOR, $result[ 1 ]);
        $this->assertEquals(getcwd() . DIRECTORY_SEPARATOR . 'blar' . DIRECTORY_SEPARATOR, $result[ 2 ]);
    }

    public function testSetTemplateDir10()
    {
        $this->smarty->setTemplateDir(array('foo', 'bah'));
        $result = $this->smarty->getTemplateDir();
        $this->assertEquals(2, count($result));
        $this->smarty->addTemplateDir(array('blar', 'smarty'));
        $result = $this->smarty->getTemplateDir();
        $this->assertEquals(4, count($result));
        $this->assertEquals(getcwd() . DIRECTORY_SEPARATOR . 'foo' . DIRECTORY_SEPARATOR, $result[ 0 ]);
        $this->assertEquals(getcwd() . DIRECTORY_SEPARATOR . 'bah' . DIRECTORY_SEPARATOR, $result[ 1 ]);
        $this->assertEquals(getcwd() . DIRECTORY_SEPARATOR . 'blar' . DIRECTORY_SEPARATOR, $result[ 2 ]);
        $this->assertEquals(getcwd() . DIRECTORY_SEPARATOR . 'smarty' . DIRECTORY_SEPARATOR, $result[ 3 ]);
    }

}
