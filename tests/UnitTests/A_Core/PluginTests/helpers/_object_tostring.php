<?php

class _object_toString
{
    protected $string = null;

    public function __construct($string)
    {
        $this->string = (string) $string;
    }

    public function __toString()
    {
        return $this->string;
    }
}

class _object_noString
{
    protected $string = null;

    public function __construct($string)
    {
        $this->string = (string) $string;
    }
}
