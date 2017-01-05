module Fluent
  module Counter
    class BaseSocket < Coolio::TCPSocket
      include Fluent::MessagePackFactory::Mixin

      def packed_write(data)
        write pack(data)
      end

      def on_read(data)
        msgpack_unpacker.feed_each(data) do |d|
          on_message d
        end
      end

      def on_message(data)
        raise NotImplementedError
      end

      private

      def pack(data)
        msgpack_packer.pack(data)
      end
    end
  end
end
