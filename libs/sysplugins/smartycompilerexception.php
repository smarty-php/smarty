<?php

/**
 * Smarty compiler exception class
 *
 * @package Smarty
 */
class SmartyCompilerException extends SmartyException
{
    /**
     * The constructor of the exception
     *
     * @param string         $message  The Exception message to throw.
     * @param int            $code     The Exception code.
     * @param string|null    $filename The filename where the exception is thrown.
     * @param int|null       $line     The line number where the exception is thrown.
     * @param Throwable|null $previous The previous exception used for the exception chaining.
     */
    public function __construct(
        string $message = "",
        int $code = 0,
        ?string $filename = null,
        ?int $line = null,
        Throwable $previous = null
    ) {
        parent::__construct($message, $code, $previous);

        // These are optional parameters, should be be overridden only when present!
        if ($filename) {
            $this->file = $filename;
        }
        if ($line) {
            $this->line = $line;
        }
    }

    /**
     * @return string
     */
    public function __toString()
    {
        return ' --> Smarty Compiler: ' . $this->message . ' <-- ';
    }
}
