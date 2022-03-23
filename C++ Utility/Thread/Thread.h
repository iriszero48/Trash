#pragma once

#include <condition_variable>
#include <mutex>
#include <list>
#include <optional>
#include <atomic>

namespace Thread
{
    constexpr int Version[]{ 1, 0, 0, 0 };
	
    template<typename Func>
    struct Synchronize
    {
        std::mutex Mtx{};
        Func F;

        explicit Synchronize(const Func& func) : F(func) {}

        template<typename ...Args>
        decltype(auto) operator()(Args&&...args)
        {
            std::lock_guard lock(Mtx);
            return F(std::forward<Args>(args)...);
        }
    };

    template<typename T>
    class Channel
    {
    public:
        void Write(const T& data)
        {
            std::unique_lock lock(mtx);
            buffer.push_back(data);
            lock.unlock();
            cv.notify_all();
        }

        T Read()
        {
            std::unique_lock lock(mtx);
            cv.wait(lock, [&]() { return !buffer.empty(); });
            auto item = buffer.front();
            buffer.pop_front();
            return item;
        }

        [[nodiscard]] auto Length() const
        {
            return buffer.size();
        }

    private:
        std::list<T> buffer{};
        std::mutex mtx{};
        std::condition_variable cv{};
    };

    template <typename T>
    class Stack
    {
        struct Node
        {
            T Data;
            Node* Next;
        };

        struct TagNode
        {
            int Tag;
            Node* Head;
        };

        std::atomic<TagNode> head = TagNode{ 0, nullptr };
    public:
        void Push(const T& value)
        {
            TagNode next = TagNode{};
            TagNode orig = head.load(std::memory_order_relaxed);
            Node* node = new Node{};
            node->Data = value;
            do {
                node->Next = orig.Head;
                next.Head = node;
                next.Tag = orig.Tag + 1;
            } while (!head.compare_exchange_weak(orig, next,
                std::memory_order_release,
                std::memory_order_relaxed));
        }

        std::optional<T> Pop()
        {
            TagNode next = TagNode{};
            TagNode orig = head.load(std::memory_order_relaxed);
            do {
                if (orig.Head == nullptr) return std::nullopt;
                next.Head = orig.Head->Next;
                next.Tag = orig.Tag + 1;
            } while (!head.compare_exchange_weak(orig, next,
                std::memory_order_release,
                std::memory_order_relaxed));
            const auto res = orig.Head->Data;
            delete orig.Head;
            return res;
        }
    };
}
