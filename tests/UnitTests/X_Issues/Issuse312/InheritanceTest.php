<?php

//require_once '../../../../../vendor/autoload.php';

class Base
{
    const TEMPLATE='034_parent.tpl';

    public $id=null;

    public function __construct($id)
    {
        $this->id=$id;
    }

    public function getHTML()
    {
        return static::class;
    }
}

class Child extends Base
{
    const TEMPLATE='034_child.tpl';

    public function getText()
    {
        return $this->getHTML();
    }
}

class BaseClone extends Base
{
    
}


/*
 * @requires PHP 5.5
 */
class InheritanceTest extends PHPUnit_Framework_TestCase
{
    protected $smarty;

    protected function setUp()
    {
        chdir(dirname(__FILE__));
        $this->smarty=new Smarty();
        $this->smarty->setTemplateDir('./templates');
        $this->smarty->setCompileDir('./templates_c');
        $this->smarty->setCacheDir('./cache');
        $this->smarty->caching=false;
    }

    /**
     * @dataProvider providerInheritance
     */
    public function testInheritance($elements, $assertions)
    {
        foreach ($elements as $nr=> $element)
        {
            $this->smarty->assign('e', $element);
            $this->assertSame($assertions[$nr], $this->smarty->fetch($element::TEMPLATE));
        }
    }
    /**
     * @dataProvider providerInheritance
     */
    public function testInheritanceCaching($elements, $assertions)
    {
        $this->smarty->caching = true;
        foreach ($elements as $nr=> $element)
        {
            $this->smarty->assign('e', $element);
            $this->assertSame($assertions[$nr], $this->smarty->fetch($element::TEMPLATE));
        }
    }
    /**
     * @dataProvider providerInheritance
     */
    public function testInheritanceCaching1($elements, $assertions)
    {
        $this->smarty->caching = true;
        foreach ($elements as $nr=> $element)
        {
            $this->smarty->assign('e', $element);
            $stat = $this->smarty->isCached($element::TEMPLATE);
            $this->assertSame($assertions[$nr], $this->smarty->fetch($element::TEMPLATE));
            $this->assertTrue($stat);
        }
    }

    public function providerInheritance()
    {
        return [
            [
                //(#0) This test works as expected
                [
                    new Base(1),
                    new Base(2),
                    new BaseClone(3),
                ], [
                    '1 Base',
                    '2 Base',
                    '3 BaseClone',
                ],
            ], [
                //(#1) This test works as expected
                [
                    new Child(1),
                    new BaseClone(2), //This output is right(!)
                ], [
                    '1 Child',
                    '2 BaseClone',
                ],
            ], [
                //(#2) This test fails
                [
                    new Child(1),
                    new Child(2),
                    new BaseClone(3),
                ], [
                    '1 Child',
                    '2 Child',
                    '3 BaseClone', //Here the output is "2 Child" 
                ],
            ], [
                //(#3) This test fails
                [
                    new Child(1),
                    new BaseClone(2),
                    new Child(3),
                ], [
                    '1 Child',
                    '2 BaseClone',
                    '3 Child', //Here the output is "2 Child" 
                ],
            ], [
                //(#4) This test fails
                [
                    new Child(1),
                    new Child(2),
                    new BaseClone(3),
                    new Child(4),
                ], [
                    '1 Child',
                    '2 Child',
                    '3 BaseClone', //Here the output is also "2 Child" 
                    '4 Child',
                ],
            ], [
                //(#5) This test fails
                [
                    new BaseClone(1),
                    new Child(2),
                    new Child(3),
                    new BaseClone(4),
                ], [
                    '1 BaseClone', //This output is right(!)
                    '2 Child',
                    '3 Child',
                    '4 BaseClone', //Here the output is "3 Child"
                ],
            ],
        ];
    }
}
