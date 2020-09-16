import { PlatypusVisitor } from './PlatypusVisitor';
import { BinaryVisitor } from './BinaryVisitor';
import { ExceptionVisitor } from './ExceptionVisitor';
import { ArgumentsVisitor } from './ArgumentsVisitor';
import { PrototypeVisitor } from './PrototypeVisitor';

export const visitors = [PlatypusVisitor, BinaryVisitor, ExceptionVisitor, ArgumentsVisitor, PrototypeVisitor];
